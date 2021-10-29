from drivers.base_driver import BaseDriver


class LktToolboxDriver(BaseDriver):
    """
    Custom driver that will automatically run the "lkt_toolbox" utility
    on the testcase's "test.lkt" file.
    """

    def run(self):
        self.run_and_check(['lkt_toolbox', 'test.lkt'],
                           memcheck=True, for_coverage=True)
