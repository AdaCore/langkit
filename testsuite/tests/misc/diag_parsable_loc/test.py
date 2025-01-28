"""
Regression test: check that errors are correctly emitted with the non-default
diagnostic style. It used to crash.
"""

import langkit.scripts.lkm as lkm


lkm.main_no_exit(["generate", "-Dgnu-base"])
