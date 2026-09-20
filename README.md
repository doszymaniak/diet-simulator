# OCaml Diet Simulator

This is a small terminal-based diet and weight simulation app I built in OCaml as part of my functional programming course. It lets a user create a profile, log meals and workouts, check daily summaries, and simulate how their weight could change over time based on calorie goals.

## What it does

- create a personal profile
- track daily meals and workouts
- calculate calories, carbohydrates, proteins, and fats
- generate daily or date-range reports
- simulate simple weight changes over time
- test a few "what if" calorie scenarios
- load user data from a JSON file

## Project structure

- `Types` — domain data types
- `Calories` — calorie intake and total calculations
- `Report` — daily and range reporting
- `User` — user creation and updates
- `Utils` — input validation and date helpers
- `Validate` — date validation logic
- `User_json` — loading a user profile from JSON

## Installation

Make sure you have OCaml, Dune, and `opam` installed.

Then install the dependency and build the project:

```bash
opam install yojson
opam exec -- dune build
```

Run the app:

```bash
opam exec -- _build/default/main.exe
```

## Usage

When the app starts, you can choose to:

1. load user data from a JSON file
2. enter a new user profile

After that, the menu lets you:
- add days
- add meals
- add workouts
- generate reports
- simulate weight change
- update your goal

## Example JSON

The sample file is stored in `data/user.json` and looks like this:

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

## Possible improvements

Possible extensions for this project include:

- saving user data automatically instead of relying on manual input
- improving meal and workout editing flow
- adding clearer report summaries and formatting
- making the CLI easier to navigate and more user-friendly
- expanding the simulation logic with more realistic health metrics