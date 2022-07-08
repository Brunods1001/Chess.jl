1. Represent chess
2. Define rules
3. Move pieces
4. Make a multiplayer game

Should Color be an enum and struct fields or a trait with variants and methods?
- methods can be overriden whereas fields are properties
- best to have fields to maintain immmutability
- i can still use the trait to define one function for all pieces?
- nah, lets try traits... revisit enums later and compare...
WAIT! Color can't be a trait... because each piece type can have multiple colors.... Traits are good for defining behavior/properties on an entire TYPE.... traits are good for discerning between types, not within them...
Chess piece colors are properties of each chess piece object!
A BETTER WAY TO USE STRUCTS IS TO CREATE AN ICON STRUCT
Traits are good for sharing behavior across types.... for one-off situations like this chess game, it's overkill!
- for example: have a BuoyancyTrait and attach it to Floats and Ship types!
