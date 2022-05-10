import glob

import langkit

from utils import emit_and_print_errors


# Compile all *.lkt" file except "common.lkt", which contains just common code
# for the other sources.
tests = [f for f in glob.glob("*.lkt") if f != "common.lkt"]


for lkt_file in sorted(tests):
    print('== {} =='.format(lkt_file))
    emit_and_print_errors(lkt_file=lkt_file, types_from_lkt=True)
    langkit.reset()
    print('')

print('Done')
