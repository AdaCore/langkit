import glob

import langkit

from utils import emit_and_print_errors


for lkt_file in sorted(glob.glob("*.lkt")):
    print(f"== {lkt_file} ==")

    emit_and_print_errors(lkt_file=lkt_file, types_from_lkt=True)
    langkit.reset()
    print("")


print("Done")
