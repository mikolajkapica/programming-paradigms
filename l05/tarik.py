import random
import sys


def simulate(win_counter, trials, switch=True):
    # 1. Randomly assign a car to one of the three doors
    car = random.randint(1, 3)

    # 2. Pick door nr 1
    door = 1

    # 3. Host opens an empty door (not door nr 1)
    if car == 1:
        host = random.randint(2, 3)
    elif car == 2:
        host = 3
    else: 
        host = 2

    # 4. Switch to the other door
    if (switch):
        if door == 1 and host == 2:
            door = 3
        elif door == 1 and host == 3:
            door = 2

    # 5. Save the result (win or lose)
    if door == car:
        return (win_counter + 1, trials + 1)
    return (win_counter, trials + 1)
    
if __name__ == "__main__":
    trials_max = 16_999
    sys.setrecursionlimit(trials_max + 10)
    def aux(win_counter, trials, switch):
        if trials == trials_max:
            return (win_counter, trials)
        return aux(simulate(win_counter, trials, switch)[0], trials + 1, switch)
    (win_counter, trials) = aux(0, 0, True)
    print("Win ratio: ", win_counter / trials)

