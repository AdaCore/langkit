from __future__ import absolute_import, division, print_function

import os.path

from langkit.template_utils import common_renderer


class LangkitSupport(object):
    """
    Helper class to generate a project file to build Langkit_Support.
    """

    def __init__(self, build_dir):
        self.build_dir = build_dir

    @property
    def lksp_project_file(self):
        """
        Absolute path to the generated project file for Langkit_Support.
        """
        return os.path.join(self.build_dir, 'lib', 'gnat',
                            'langkit_support.gpr')

    @property
    def lksp_source_dir(self):
        """
        Absolute path to the directory that contains Langkit_Support source
        files.
        """
        return os.path.join(os.path.dirname(os.path.abspath(__file__)),
                            'support')

    def generate(self):
        """
        Generate the build tree and the project file.
        """
        build_dir = self.build_dir
        lib_dir = os.path.join(build_dir, 'lib')
        lib_gnat_dir = os.path.join(lib_dir, 'gnat')

        for d in [build_dir, lib_dir, lib_gnat_dir]:
            if not os.path.isdir(d):
                os.mkdir(d)

        with open(self.lksp_project_file, 'w') as f:
            f.write(common_renderer.render('langkit_support_gpr',
                                           source_dir=self.lksp_source_dir))
