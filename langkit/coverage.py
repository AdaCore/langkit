"""
Generation of code coverage reports for generated libraries.
"""

from __future__ import absolute_import, division, print_function

from collections import OrderedDict
import glob
import json
import os.path
import shutil
import subprocess
import xml.etree.ElementTree as etree

from langkit.gdb.debug_info import DebugInfo, ExprStart
from langkit.template_utils import Renderer
from langkit.utils import ensure_clean_dir


css_file = os.path.join(os.path.dirname(os.path.realpath(__file__)),
                        'coverage.css')
"""
Path to the CSS file to use in coverage reports.
"""


def html_escape(content):
    """
    Escape ``content`` to include it in HTML documents as text.

    The "html" module became available starting with Python 3.2. We support
    Python 2 for now, so use a substitute in the meantime.
    """
    substitutions = [('&', '&amp;'),
                     ('<', '&lt;'),
                     ('>', '&gt;'),
                     ('"', '&quot;')]
    for pattern, replacement in substitutions:
        content = content.replace(pattern, replacement)
    return content


class InstrumentationMetadata(object):
    """
    Holder for metadata produced during source code instrumentation and
    required in order to produce final coverage reports.

    These two steps happen in different processes, so this holder is useful to
    transmit information that is costly to recompute using the filesystem.
    """

    MAGIC = 'langkit-instrumentation-metadata'
    """
    Magic string to identify the metadata file format.
    """

    CURRENT_VERSION = 1
    """
    Version number for the metadata file format. Used to clearly reject
    obsolete metadata files instead of waiting for obscure errors happening.
    """

    def __init__(self):
        self.additional_sources = set()
        self.generated_sources = set()

    @staticmethod
    def _filename(instr_dir):
        return os.path.join(instr_dir, 'instr-metadata.json')

    def save(self, instr_dir):
        with open(self._filename(instr_dir), 'w') as f:
            json.dump({
                'version': self.CURRENT_VERSION,
                'type': self.MAGIC,
                'additional_sources': list(self.additional_sources),
                'generated_sources': list(self.generated_sources),
            }, f)

    @classmethod
    def load(cls, instr_dir):
        result = cls()

        with open(cls._filename(instr_dir), 'r') as f:
            md = json.load(f)

        if md.get('type') != cls.MAGIC:
            raise ValueError('Invalid instrumentation metadata file')

        version = md.get('version')
        if version != cls.CURRENT_VERSION:
            raise ValueError('Unexpected version number for instrumentation'
                             ' metadata: got {} but expected {}'
                             .format(version, cls.CURRENT_VERSION))

        result.additional_sources.update(md['additional_sources'])
        result.generated_sources.update(md['generated_sources'])
        return result


class CoverageReport(object):
    """
    Code coverage report.
    """

    class Group(object):
        """
        Group of several source file coverage report.
        """
        def __init__(self, name, label):
            self.name = name
            self.label = label
            self.files = {}

    class File(object):
        """
        Coverage report for a single file.
        """
        def __init__(self, name):
            self.name = name
            self.lines = []
            self._summary = None

        @property
        def summary(self):
            if self._summary is None:
                total = 0
                self._summary = {state: 0
                                 for state in CoverageReport.SUMMARY_STATES}
                for l in self.lines:
                    if l.state in CoverageReport.SUMMARY_STATES:
                        total += 1
                        self._summary[l.state] += 1
                if total:
                    for state in CoverageReport.SUMMARY_STATES:
                        self._summary[state] /= float(total)
            return self._summary

        @property
        def html_file(self):
            return self.name + '.html'

    class Line(object):
        """
        Coverage report for a single line.
        """
        def __init__(self, lineno, content, state):
            self.lineno = lineno
            self.content = content
            self.state = state
            self.annotations = []

    class Annotation(object):
        """
        Coverage annotation related to a line.
        """
        def __init__(self, kind, message):
            self.kind = kind
            self.message = message

    UNKNOWN_STATE_NAME = 'unknown'
    STATES = [
        ('.', 'no_code'),
        ('+', 'covered'),
        ('!', 'partially_covered'),
        ('-', 'uncovered'),
        ('?', 'unknown'),
    ]
    STATE_TO_NAME = dict(STATES)

    # Leave no-code lines out of the summary picture: they are not relevant
    SUMMARY_STATES = [s for s, _ in STATES if s != '.']

    @classmethod
    def state_name(cls, state):
        return cls.STATE_TO_NAME.get(state, cls.UNKNOWN_STATE_NAME)

    def __init__(self, title):
        self.title = title
        self.groups = OrderedDict()

    def get_or_create(self, group_name, group_label):
        try:
            return self.groups[group_name]
        except KeyError:
            result = CoverageReport.Group(group_name, group_label)
            self.groups[group_name] = result
            return result

    def import_gnatcov_xml(self, xml_dir):
        """
        Read source file coverage reports from a gnatcov XML report.

        :param str xml_dir: Output directory for "gnatcov coverage" when it
            produced the XML report.
        """

        def load_xml(filename):
            with open(os.path.join(xml_dir, filename), 'r') as f:
                return etree.parse(f).getroot()

        def get_child(root, tag):
            for child in root:
                if child.tag == tag:
                    return child
            assert False

        result = []

        # Get the list of file reports from the index file
        index = load_xml('index.xml')
        assert index.tag == 'document'
        summary = get_child(get_child(index, 'coverage_report'),
                            'coverage_summary')
        files = [f.attrib['name'] for f in summary if f.tag == 'file']

        # Read the coverage report for each file
        for f in files:
            file_report = CoverageReport.File(f)

            # Parse all lines
            for src_mapping in load_xml(f + '.xml'):
                xml_line = get_child(get_child(src_mapping, 'src'), 'line')
                line = CoverageReport.Line(int(xml_line.attrib['num']),
                                           xml_line.attrib['src'],
                                           src_mapping.attrib['coverage'])
                file_report.lines.append(line)

                # Parse line messages
                for message in src_mapping:
                    if message.tag != 'message':
                        continue
                    line.annotations.append(CoverageReport.Annotation(
                        message.attrib['kind'],
                        message.attrib['message']
                    ))
            result.append(file_report)

        return result

    def render(self, output_dir):
        def out_path(filename):
            return os.path.join(output_dir, os.path.basename(filename))

        shutil.copyfile(css_file, out_path(css_file))

        r = Renderer(report=self,
                     escape=html_escape,
                     state_name=self.state_name)

        # Output the index
        with open(out_path('index.html'), 'w') as f:
            f.write(r.render('coverage/index_html'))

        # Output one page per reported file
        for group in self.groups.values():
            for src_file in group.files.values():
                report_filename = out_path(src_file.html_file)
                with open(report_filename, 'w') as f:
                    f.write(r.render('coverage/file_html', src_file=src_file))


class PropertyDSLCoverage(object):
    """
    Helper to compute the coverage of the property DSL.
    """

    class Data(object):
        """
        Coverage data for a DSL expression.
        """

        def __init__(self, expr):
            self.expr = expr
            self.has_code = False
            self.covered = False

        @property
        def state(self):
            if self.has_code:
                return '+' if self.covered else '-'
            else:
                return '.'

    def __init__(self, input_file, report_group):
        """
        Parse GDB helpers directives in the ``input_file`` coverage
        report, decode property DSL-level coverage from it and add this
        coverage information to ``report_group``.

        :param CoverageReport.File file_report: File coverage report to read.
        :param CoverageReport.Group report_group: Group of coverage reports
            under which DSL files should go.
        """
        self.input_file = input_file
        self.report_group = report_group
        self.debug_info = DebugInfo.parse_from_iterable(
            filename=input_file.name,
            lines=(line.content for line in input_file.lines)
        )

        self.gen_to_cov = [[] for _ in self.input_file.lines]
        """
        :type: list[list[PropertyDSLCoverage.Data]]

        For each line in self.input_file (the generated source), list of
        coverage data for expressions that apply to this line.
        """

        self.orig_to_cov = {}
        """
        :type: dict[str, list[list[PropertyDSLCoverage.Data]]]

        For each line in each original source files (source file names are dict
        keys), list of coverage data for scopes that apply to this line.
        """

        self.map_lines()
        self.annotate()
        self.propagate()

    def open_orig_file(self, filename):
        """
        Consider that ``filename`` is an original source file: if this file is
        unknown so far, create a coverage report for it and start mapping its
        lines to expressions. Return this mapping (it's an item in
        self.orig_to_cov).

        :rtype: list[list[PropertyDSLCoverage.Data]]
        """
        name = os.path.basename(filename)

        # Create a coverage report for filename if there is none
        try:
            file_report = self.report_group.files[name]
        except KeyError:
            file_report = CoverageReport.File(name)
            self.report_group.files[name] = file_report

            # Load the content of the file and consider for starters that no
            # line has associated code.
            with open(filename, 'r') as f:
                for lineno, line in enumerate(f, 1):
                    file_report.lines.append(CoverageReport.Line(
                        lineno, line, '.'
                    ))

        # If this is the first time we see this filename, map its line to DSL
        # expressions. This can be the first time even though we already had a
        # coverage report for this file, as in theory several generated file
        # can refer to the same original file.
        try:
            return self.orig_to_cov[name]
        except KeyError:
            result = [[] for _ in file_report.lines]
            self.orig_to_cov[name] = result
            return result

    def map_lines(self):
        """
        Map lines in original and generated sources to abstract DSL constructs
        (scopes).
        """
        for prop in self.debug_info.properties:
            for expr in prop.iter_events(filter=ExprStart):
                # Silently ignore sloc-less expressions, as we can do nothing
                # with them. These are probably artificial expressions (i.e.
                # created during compilation but not coming from sources)
                # anyway, so not relevant to coverage analysis.
                if not expr.dsl_sloc:
                    continue

                data = PropertyDSLCoverage.Data(expr)

                # Map DSL linenos to DSL coverage data
                orig_to_cov = self.open_orig_file(expr.dsl_sloc.filename)
                orig_to_cov[expr.dsl_sloc.line_no - 1].append(data)

                # Map generated code linenos to DSL coverage data
                line_range = expr.line_range
                for lineno in range(line_range.first_line,
                                    line_range.last_line + 1):
                    self.gen_to_cov[lineno - 1].append(data)

    def annotate(self):
        """
        Use mappings to to convert coverage of generated sources to coverage of
        DSL expressions.
        """
        for lineno, line in enumerate(self.input_file.lines, 1):
            has_code, covered = {
                '.': (False, False),
                '+': (True, True),
                '!': (True, True),
                '-': (True, False),

                # Be conservative: if we find some unknown coverage state,
                # consider we have code and that coverage is not reached so
                # that users feel the need to investigate.
                '?': (True, False),
            }[line.state]

            if has_code or covered:
                for data in self.gen_to_cov[lineno - 1]:
                    data.has_code = data.has_code or has_code
                    data.covered = data.covered or covered

    def propagate(self):
        """
        Propagate coverage of DSL expressions to original source report.
        """
        # Set of expression for which we emitted a violation. Expressions can
        # span over multiple lines, so we don't want to emit one violation per
        # line.
        violation_emitted = set()

        for name, orig_to_cov in self.orig_to_cov.items():
            file_report = self.report_group.files[name]
            for lineno, line in enumerate(file_report.lines, 1):
                for data in orig_to_cov[lineno - 1]:
                    # Transition the coverage state for this line to account
                    # for covered/has_code annotations.
                    if data.covered:
                        line.state = {
                            '.': '+',
                            '+': '+',
                            '!': '!',
                            '-': '!',
                            '?': '!',
                        }[line.state]
                    elif data.has_code:
                        line.state = {
                            '.': '-',
                            '+': '!',
                            '!': '!',
                            '-': '-',
                            '?': '?',
                        }[line.state]

                        # This expression has code and is not covered: if not
                        # done already, emit a violation for it.
                        if data.expr not in violation_emitted:
                            violation_emitted.add(data.expr)
                            line.annotations.append(CoverageReport.Annotation(
                                'violation',
                                '{} not executed'.format(data.expr.expr_repr)
                            ))


class GNATcov(object):
    """
    Simple wrapper around the "gnatcov" tool.
    """

    # Only do statement coverage
    covlevel = 'stmt'

    def __init__(self, context=None):
        self.context = context

    def _unit_slug(self, base_filename):
        """
        Return the slug that "gnatcov instrument" computes for a source file.

        See GNATcoverage's instrument-common.ads for more information.

        :rtype: str
        """
        # Identify which unit "filename" is for. Pray that there are no
        # separates in the generated libraary.
        unit_name = os.path.splitext(base_filename)[0]
        is_spec = base_filename.endswith('.ads')

        return '{}_{}'.format(
            's' if is_spec else 'b',
            '_z_'.join(part.replace('z', 'zz')
                       for part in unit_name.split('-'))
        )

    def buffer_list_file(self, emitter):
        """
        Return the name of the source file that "gnatcov instrument" creates to
        hold the list of coverage buffers for the generated library.
        """
        return 'gnatcov_rts-buffers-lists-{}.ads'.format(emitter.lib_name_low)

    def buffer_files(self, base_filename):
        """
        Return the names of the source files that "gnatcov instrument" creates
        to hold coverage buffers corresponding to the given file name.

        Note: subunits are not supported.

        :param str base_filename: Base filename for which we want coverage
            buffer source files.
        :rtype: list[str]
        """
        unit_slug = self._unit_slug(base_filename)
        return ['gnatcov_rts-buffers-{}{}.ads'.format(buffer_kind, unit_slug)
                for buffer_kind in ('p', 'b')]

    def instrument(self, emitter, instr_dir):
        """
        Run "gnatcov instrument" on the generated library.

        :param str instr_dir: Directory used to store instrumentation data,
            i.e. data produced by instrumentation and required to produce
            coverage reports.

        Put SID files in the ``$BUILD_DIR/obj/$LIBNAME/sids`` directory
        (removed and created if needed).
        """
        ensure_clean_dir(instr_dir)

        subprocess.check_call([
            'gnatcov', 'instrument',
            '--level', self.covlevel,
            '-P', emitter.main_project_file,
            '-X{}_COVINSTR=true'.format(emitter.lib_name_up)
        ])

        # At this point, instrumented sources are located in the object
        # directory, which depends on the build mode: relocate it somewhere
        # else (i.e. rename to instr_dir) so that the same set of instrumented
        # sources applies to all builds.
        lib_obj_dir = os.path.join(emitter.lib_root, 'obj',
                                   emitter.lib_name_low)
        instr_src_dir = os.path.join(lib_obj_dir, 'gnatcov-instr')
        if os.path.exists(instr_src_dir):
            shutil.rmtree(instr_src_dir)
        os.rename(os.path.join(lib_obj_dir, 'dev', 'gnatcov-instr'),
                  instr_src_dir)

        # "gnatcov instrument" instruments only Ada sources, so we need to
        # manually copy the C sources (if any).
        lib_src_dir = os.path.join(emitter.lib_root, 'include',
                                   emitter.lib_name_low)
        for pattern in ('*.c', '*.h'):
            for f in glob.glob(os.path.join(lib_src_dir, pattern)):
                shutil.copyfile(f, os.path.join(instr_src_dir,
                                                os.path.basename(f)))

        # Create a directory to gather all SID files
        sid_dir = os.path.join(instr_dir, 'sids')
        ensure_clean_dir(sid_dir)
        for f in glob.glob(os.path.join(lib_obj_dir, '*', '*.sid')):
            shutil.copyfile(f, os.path.join(sid_dir, os.path.basename(f)))

        # Create instrumentation metadata
        emitter.instr_md.save(instr_dir)

    def _generate_xml_report(self, gnatcov_args, instr_dir, traces,
                             working_dir):
        """
        Helper for generate_report. Run "gnatcov run" to produce a XML report.

        :rtype: str
        :return: Output directory for the XML report.
        """
        # Compute the list of SID files
        sid_dir = os.path.join(instr_dir, 'sids')
        sid_list = os.path.join(working_dir, 'sids.txt')
        with open(sid_list, 'w') as f:
            for t in glob.glob(os.path.join(sid_dir, '*.sid')):
                f.write(t + '\n')

        # Compute the list of traces files
        trace_list = os.path.join(working_dir, 'traces.txt')
        with open(trace_list, 'w') as f:
            for t in traces:
                f.write(t + '\n')

        # Compute a first coverage report, for all Ada source files
        xml_dir = os.path.join(working_dir, 'xml-report')
        ensure_clean_dir(xml_dir)
        subprocess.check_call(
            ['gnatcov', 'coverage',
             '--level', self.covlevel,
             '--annotate', 'xml',
             '--output-dir', xml_dir,
             '--sid', '@' + sid_list,
             '@' + trace_list] + gnatcov_args
        )
        return xml_dir

    def _generate_final_report(self, title, instr_dir, xml_dir, output_dir):
        """
        Helper for generate_report. Load GNATcoverage's XML report and produce
        our final coverage report.
        """
        # Read instrumentation metadata
        instr_md = InstrumentationMetadata.load(instr_dir)

        # Load the XML report
        report = CoverageReport(title)
        orig_sources = report.get_or_create('orig', 'Original sources')
        gen_sources = report.get_or_create('gen', 'Generated sources')
        for f in report.import_gnatcov_xml(xml_dir):
            if f.name in instr_md.additional_sources:
                group = orig_sources
            elif f.name in instr_md.generated_sources:
                group = gen_sources
            else:
                group = report.get_or_create('unknown', 'Unknown sources')
            group.files[f.name] = f

        # Create property DSL-level coverage info from the coverage reports of
        # generated sources.
        for f in gen_sources.files.values():
            PropertyDSLCoverage(f, orig_sources)

        # Output the final report
        report.render(output_dir)

    def generate_report(self, title, gnatcov_args, instr_dir, traces,
                        output_dir, working_dir):
        """
        Generate a HTML coverage report.

        :param str title: Title for the coverage report.
        :param list[str] gnatcov_args: Command-line arguments to forward to
            gnatcov. These should convey the list of units of interest, so
            either pass a project file, either give the list of SID files.
        :param str instr_dir: Directory that contains instrumentation data (see
            the corresponding argument in the "instrument" method).
        :param listr[str] traces: List of source trace files to discharge
            coverage obligations. Typically: execution traces from a testsuite.
        :param str output_dir: Path to the directory where gnatcov will
            output the coverage report. Beware, this removes this directory if
            it exists.
        :param str working_dir: Temporary directory.
        """
        # Make sure we start with a clean output directory
        ensure_clean_dir(output_dir)

        xml_dir = self._generate_xml_report(gnatcov_args, instr_dir, traces,
                                            working_dir)
        self._generate_final_report(title, instr_dir, xml_dir, output_dir)