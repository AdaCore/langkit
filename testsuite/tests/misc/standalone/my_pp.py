from langkit.utils import SourcePostProcessor


HEADER = "--  CUSTOM HEADER\n"


class AdaSourcePostProcessor(SourcePostProcessor):
    def process(self, source):
        return HEADER + source
