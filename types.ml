type goal =
| Reduce of float
| Maintain
| Gain of float


type gender =
| Male
| Female


type meal =
{
  calories : int;
  proteins : int;
  carbohydrates : int;
  fats : int;
}


type day =
{
  breakfast : meal option;
  snack_1 : meal option;
  lunch : meal option;
  snack_2 : meal option;
  dinner : meal option;
  workout: int option;
}


type user =
{
  name : string;
  age : int;
  gender : gender;
  weight : float;
  height : int;
  goal : goal;
  pal : float;
  calorie_intake : float;
  days : day list
}