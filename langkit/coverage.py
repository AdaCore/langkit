"""
Generation of code coverage reports for generated libraries.
"""

from __future__ import absolute_import, division, print_function

import glob
import os.path
import shutil
import subprocess

from langkit.utils import ensure_clean_dir


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

    def generate_report(self, title, gnatcov_args, instr_dir, traces,
                        output_dir, working_dir):
        """
        Run "gnatcov coverage" to generate a DHTML coverage report.

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

        # Compute the list of SID files
        sid_dir = os.path.join(instr_dir, 'sids')
        sid_list = os.path.join(working_dir, 'sids.txt')
        with open(sid_list, 'w') as f:
            for t in glob.glob(os.path.join(sid_dir, '*.sid')):
                f.write(t + '\n')

        # Compute the list of traces files
        trace_list = os.path.join(output_dir, 'traces.txt')
        with open(trace_list, 'w') as f:
            for t in traces:
                f.write(t + '\n')

        subprocess.check_call(
            ['gnatcov', 'coverage',
             '--level', self.covlevel,
             '--annotate', 'dhtml',
             '--output-dir', output_dir,
             '--report-title', title,
             '--sid', '@' + sid_list,
             '@' + trace_list] + gnatcov_args
        )
