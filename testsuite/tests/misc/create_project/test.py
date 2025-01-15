"""
Test that the "create-project.py" script creates a valid Langkit project by
generating it and building it (expecting no warning).
"""

import langkit.scripts.create_project as create_project
import langkit.scripts.lkm as lkm


create_project.main(["Mylang"])
lkm.main(["make", "-vnone", "--enable-build-warnings"])

print('Done')
