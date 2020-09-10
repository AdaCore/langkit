import libfoolang as lfl


class App(lfl.App):
    def process_unit(self, unit):
        unit.root.dump()


if __name__ == '__main__':
    App.run(['input3', 'input1', 'input2'])
