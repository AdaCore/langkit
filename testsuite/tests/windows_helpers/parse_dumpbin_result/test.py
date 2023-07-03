import os

from langkit.windows import parse_dumpbin_result


# Global list to store all parsing results
parsing_results = {}
base_version = "2013"

# Parse all dumpbin out files
for version in ["2013", "2022"]:
    with open(f"dumpbin_{version}.out", 'r') as dumpbin_out_file:
        parsing_results[version] = parse_dumpbin_result(
            dumpbin_out_file.read()
        )

# Print the symbols found in the base version
base_result = parsing_results[base_version]
print(f"--- Read symbols from dumpbin {base_version}")
print(*base_result, sep=os.linesep)
print()

# Explore all other version and display the difference
for version, result in parsing_results.items():
    if version == base_version:
        continue

    # Compute the difference between base result and current result
    if result != base_result:
        base_result_tmp = base_result.copy()
        print(f"Different symbols in {version} (compared to {base_version}):")
        for symbol in result:
            if symbol not in base_result_tmp:
                print(f"  +{symbol}")
            else:
                base_result_tmp.remove(symbol)
        for symbol in base_result_tmp:
            print(f"  -{symbol}")
        print()
