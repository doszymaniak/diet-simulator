# OCaml Diet Simulator 🥗📊
This is a working version of my mini terminal-based diet and weight simulation program written in OCaml. It was created as part of my functional programming course at the university.
## Features
This program allows users to:
- create a personal profile
- keep track of daily meals and workouts
- calculate total calories, macronutrients, and calories burned
- generate daily or date-range reports
- simulate simple weight changes over time, including the "what-if" scenarios
- load user data from JSON files
## Modules
- `Types` - domain data types
- `Calories` - functions to calculate calorie intake and totals
- `Report` - daily and range reporting functionality
- `User` - user creation, updating, meal and workout modification
- `Utils` - safe input handling, date comparison, day selection
- `Validate` - date validation
- `User_json` - loading user data from JSON
## Installation
Make sure you have **OCaml** and **Dune** installed.
1. Clone the repository
```bash
git clone https://github.com/doszymaniak/diet-simulator.git
cd diet-simulator
```
2. Build the project using Dune
```bash
dune build
```
3. Run the program
```bash
_build/default/main.exe
```
## Usage
Upon running, you can choose to:
1. Load user data from a JSON file
2. Enter a new user profile

Then, use the menu to navigate through all functionalities.
## JSON format
Example JSON for loading a user (stored in `data/` folder):
```json
{
    "name": "Anna",
    "age": 20,
    "gender": "Female",
    "weight": 64.2,
    "height": 168,
    "goal": {"Reduce": 60.0},
    "pal": 1.3,
    "calorie_intake": 1845
}
```