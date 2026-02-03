type goal =
| Reduce of float
| Maintain
| Gain of float


type gender =
| Male
| Female


type day =
{
  breakfast : int;
  lunch : int;
  dinner : int;
  workout: int option
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