pub use parser::internal::{input,values,storage,recipes,fluid};

peg! internal(r#"
    use data::Storage;
    use data::Values;
    use data::{Recipes,Recipe};

    #[pub]
    input -> (Values, Storage, Recipes) =
    	v:values nl s:storage nl r:recipes nl f:fluid nl { (v, s.set_fluid(f), r) }

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
