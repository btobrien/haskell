
#include <optional>
#include <bitset>
#include <iostream>
#include <vector>
#include <sstream>

constexpr int FLOORS = 10;
constexpr int ELEVATORS = 4;

enum Direction { UP, DOWN };
std::string to_string(Direction d) { return d == UP ? "UP" : "DOWN"; }

class Elevator {
public:

    Elevator() {}
    Elevator(int id, int floor = 0) : id(id), floor(floor) {}

    void Tick() {
        if (IsIdle()) return;

        if (floorRequests[floor]) {
            floorRequests[floor] = false;
            inReversePickup = false;
            OpenDoor();
            // don't move if we opened the door
            return;
        }

        if (floor == FLOORS - 1) {
            direction = DOWN;
        }
        else if (floor == 0) {
            direction = UP;
        }

        if (direction == UP) {
            floor++;
        }
        else if (direction == DOWN) {
            floor--;
        }
    }

    bool IsIdle() const {
        return !floorRequests.any();
    }

    // returns true if the elevator is going in the right direction, relative to the floors
    // (note: copilot wrote this entire function and both these comments...)
    bool IsPassing(int floor, Direction direction) const {
        if (IsIdle() || inReversePickup) return false;
        if (this->direction != direction) return false;
        if (direction == UP) {
            return this->floor <= floor;
        }
        else if (direction == DOWN) {
            return this->floor >= floor;
        }
        return false;
    }

    void RequestFloor(int floor, Direction direction) {
        RequestFloor(floor);
        inReversePickup = direction != this->direction; // won't be able to pick anyone up in passing
    }

    void RequestFloor(int floor) {
        floorRequests[floor] = true;
        direction = floor > this->floor ? UP : DOWN;
    }

    int GetFloor() const {
        return floor;
    }

private:
    int id;
    int floor;
    Direction direction;
    std::bitset<FLOORS> floorRequests;
    bool inReversePickup = false; // special case required when picking up someone from idle that will require us to reverse afterwards
                                  // important becaues it means we won't be able to pick anyone up in passing

    virtual void OpenDoor() {
        std::cout << "door is open on elevator " << id << " going " << (IsIdle() ? "?" : to_string(direction)) << " from floor " << floor << std::endl;
    }

};

class ElevatorSystem {
    private:
        Elevator elevators[ELEVATORS];

        Elevator* SelectElevator(int floor, Direction direction) {
            Elevator* bestElevator = nullptr;
            int bestDistance = FLOORS;
            for (auto& elevator : elevators) {
                if (elevator.IsIdle() || elevator.IsPassing(floor, direction)) {
                    int distance = std::abs(elevator.GetFloor() - floor);
                    // crazy thing is that it used GetFloor before I wrote it...
                    if (distance < bestDistance) {
                        bestDistance = distance;
                        bestElevator = &elevator;
                    }
                }
            }
            return bestElevator;
        }

        // store deffered requests to be reevaluated on the next tick
        std::vector<std::pair<int, Direction>> deferredRequests;
    public:
        ElevatorSystem() {
            for (int i = 0; i < ELEVATORS; i++) {
                elevators[i] = Elevator(i);
            }
        }

        void Tick() {
            for (auto& elevator : elevators) {
                elevator.Tick();
            }
            for (auto it = deferredRequests.begin(); it != deferredRequests.end();) {
                auto& [floor, direction] = *it;
                Elevator* elevator = SelectElevator(floor, direction);
                if (elevator) {
                    elevator->RequestFloor(floor, direction);
                    it = deferredRequests.erase(it);
                }
                else {
                    it++;
                }
            }
        }

        void RequestElevator(int floor, Direction direction) {
            Elevator* elevator = SelectElevator(floor, direction);
            if (elevator) {
                elevator->RequestFloor(floor, direction);
            }
            else {
                deferredRequests.push_back({ floor, direction });
            }
        }

        void RequestFloor(int elevator, int floor) {
            elevators[elevator].RequestFloor(floor);
        }
};

std::vector<std::string> split(const std::string& s) {
    std::vector<std::string> tokens;
    std::string token;
    std::istringstream tokenStream(s);
    while (std::getline(tokenStream, token, ' ')) {
        tokens.push_back(token);
    }
    return tokens;
}

int main() {
    ElevatorSystem system;
    std::string line;
    while (getline(std::cin, line))
    {
        auto command = split(line);
        if (line == "") {
            system.Tick();
        }
        else if (command.size() < 3) {
            std::cerr << "missing command args: " << line << std::endl;
        }
        else if (command[1] == "from") {

            Direction direction = command[0] == "up" ? UP : DOWN;
            int floor = std::stoi(command[2]);
            system.RequestElevator(floor, direction);
        }
        else if (command[1] == "to") {
            int elevator = std::stoi(command[0]);
            int floor = std::stoi(command[2]);
            system.RequestFloor(elevator, floor);
        }
        else {
            std::cerr << "command not recognized: " << line << std::endl;
        }
    }

    return 0;
}












































