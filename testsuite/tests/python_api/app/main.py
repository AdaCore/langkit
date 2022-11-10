import libfoolang as lfl


class App(lfl.App):
    def process_unit(self, unit):
        unit.root.dump()


class App2(lfl.App):
    def process_unit(self, unit):
        unit.root.dump()

    def default_get_files(self):
        return ['input3', 'input1', 'input2']


class App3(lfl.App):
    def process_unit(self, unit):
        unit.root.dump()


if __name__ == '__main__':
    print("Running App1")
    App.run(['input3', 'input1', 'input2'])
    print()
    print("Running App2")
    App2.run([])
    print()
    print("Running App3")
    App3.run([])
    print()
