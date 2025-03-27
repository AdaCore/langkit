from setuptools import setup, find_packages


setup(
    name="Liblktlang",
    version="0.1",
    packages=["liblktlang"],
    package_data={
        "liblktlang": [
            "*.{}".format(ext) for ext in ("dll", "so", "so.*", "dylib")
        ]
        + ["py.typed"],
    },
    zip_safe=False,
)
