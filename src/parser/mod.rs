use super::data::{Values,Storage,Recipes};

peg! internal(r#"
    use super::super::data::{Values,Storage,Recipes,Recipe};

    #[pub]
    input -> (Values, Storage, Recipes, usize) =
    	v:values nl s:storage nl r:recipes nl f:fluid nl { (v, s, r, f) }

    #[pub]
    values -> Values =
    	"[" vs:value ** "," "]" {Values::new(vs)}

    #[pub]
    storage -> Storage =
    	"[" cs:counts ** "," "]" {Storage::new(cs)}

    #[pub]
    recipes -> Recipes =
    	"[" rs:recipe ** "," "]" {Recipes::new(rs)}

    #[pub]
    fluid -> usize =
    	[0-9]+ { match_str.parse().unwrap() }

    value -> isize =
    	"-"? [0-9]+ { match_str.parse().unwrap() }

    counts -> usize =
    	[0-9]+ { match_str.parse().unwrap() }

    recipe -> Recipe =
        "(" i:item_list "," o:item_list "," f:fluid ")" { Recipe::new(i, o, f) }

    item_list -> Vec<usize> =
        "[" f:fluid ** "," "]" { f }

    nl -> () =
    	"\n" / "\r\n" / "\n\r"
"#);

pub fn input(input: &str) -> Result<(Values, Storage, Recipes, usize), internal::ParseError> {
    internal::input(input)
}

pub fn values(input: &str) -> Result<Values, internal::ParseError> {
    internal::values(input)
}

pub fn storage(input: &str) -> Result<Storage, internal::ParseError> {
    internal::storage(input)
}

pub fn recipes(input: &str) -> Result<Recipes, internal::ParseError> {
    internal::recipes(input)
}

pub fn fluid(input: &str) -> Result<usize, internal::ParseError> {
    internal::fluid(input)
}
