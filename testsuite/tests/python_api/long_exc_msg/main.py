import libfoolang


print("main.py: Running...")

try:
    libfoolang.do_something()
except libfoolang.PropertyError as exc:
    print("Got a PropertyError")
    msg = str(exc)
    print("Message size:", len(msg))
    assert msg == "A" + "B" * 9998 + "C", "Unexpected exception message"
else:
    assert False, "PropertyError expected, got none"

print("main.py: Done.")
