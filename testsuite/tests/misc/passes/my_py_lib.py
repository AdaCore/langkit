from langkit.passes import GlobalPass


def create_pass(name):
    def f(ctx):
        print(f"... execution of {name}...")

    return GlobalPass(name, f)


def pass_a():
    return create_pass("pass_a")


def pass_b():
    return create_pass("pass_b").optional("Doc for pass_b")


def pass_c():
    return create_pass("pass_c").optional(
        """
        Doc for pass_c. This documentation is very long, because it has many
        things to explain. So long that it will surely be rewrapped and thus
        allow us to test how this documentation is displayed.
        """,
        disabled=False,
    )


def invalid_cb1(a):
    return a


def invalid_cb2():
    return 1
