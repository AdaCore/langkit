import os.path


print("Implementation packages:")
for filename in sorted(os.listdir("build/src")):
    if filename.startswith("libfoolang-impl_"):
        print(filename)
print("Done")
