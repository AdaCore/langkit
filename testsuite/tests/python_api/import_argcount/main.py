import libfoolang


print("main.py: Running...")

for args, kwargs in [
    ([], {}),
    ([1], {}),
    ([], {"hello": 2}),
]:
    print("Trying to call with {} and {}...".format(args, kwargs))
    try:
        libfoolang._token_kind_name(*args, **kwargs)
    except TypeError as exc:
        print("   Got a TypeError exception: {}".format(exc))
    else:
        print("   Success")


print("main.py: Done.")
