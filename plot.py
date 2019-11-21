import matplotlib.pyplot as plt
from numpy import linspace, array


def read_file(filename):
    data = []
    with open(filename, 'r') as file:
        for i in file.readlines():
            data.append(int(i))
    return array(data)/data[0]


def main():
    data = read_file('pop_size_f08.csv')
    plt.plot(data, '.')
    plt.grid('minor')
    plt.xlim(1, len(data))
    plt.ylim(0, max(data)*1.1)

    plt.xlabel('Time step, $t$')
    plt.ylabel('Normalized pop size, $N(t) / N_0$')

    plt.savefig('./results/pop_size.png', dpi=100, bbox_inches='tight')

if __name__ == '__main__':
    main()
