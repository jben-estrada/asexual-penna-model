import matplotlib.pyplot as plt
from numpy import linspace


def read_file(filename):
    data = []
    with open(filename, 'r') as file:
        for i in file.readlines():
            data.append(int(i))
    return data


def main():
    data = read_file('pop_size_f08.csv')
    steps = linspace(1, len(data), len(data)//5 + 1, dtype=int)
    plt.plot(data, '.')
    plt.xticks(steps, rotation=45)
    plt.grid('minor')
    plt.xlim(1, len(data))
    plt.savefig('./results/pop_size.png', dpi=100, bbox_inches='tight')

if __name__ == '__main__':
    main()