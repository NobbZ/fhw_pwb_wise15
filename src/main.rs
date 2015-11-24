#![feature(convert)]

extern crate pwb_ws_15 as pwb;
extern crate rose_tree;

use rose_tree::RoseTree;
use rose_tree::NodeIndex;

use std::io;
use std::thread;

use std::sync::Arc;
use std::sync::Mutex;

use pwb::parser;
use pwb::data::Storage;
use pwb::data::Recipes;

//use pwb::data::tree::Node;

fn expand(tree: &mut RoseTree<Option<Arc<Storage>>, usize>, idx: NodeIndex<usize>, recipes: &Recipes) {
    let storage = match &tree[idx] {
        &Some(ref x) => x.clone(),
        &None => unimplemented!(),
    };
    for r in recipes.iter() {
        if let Some(new_storage) = r.produce(Arc::new((*storage).clone())) {
            let child_idx = tree.add_child(idx, Some(new_storage.clone()));
            expand(tree, child_idx, recipes);
        } else {
            tree.add_child(idx, None);
        }
    }
}

fn main() {
    let mut line = String::new();

    // That damn range is exclusive at the end... So it really means 4 times!
    for _ in 1..5 {
        let _ = io::stdin().read_line(&mut line).unwrap();
    }

    // Lets assume that there is only valid input during the contest
    let (values, storage, recipes) =
        parser::input(line.as_str()).unwrap();

    println!("{:?}", values);
    println!("{:?}", storage);
    println!("{:?}", recipes);
    println!("{:?}", storage.get_fluid());

    let (mut tree_base, root) = RoseTree::<Option<Arc<Storage>>, usize>::new(Some(Arc::new(storage.clone())));
    //let tree = Arc::new(Mutex::new(tree_base));

    expand(&mut tree_base, root, &recipes);



    //println!("{:?}", tree);
    //println!("{:?}", root);

    // let mut root = Node::new(storage, &recipes);
    // println!("{:?}", root);

    // root.eval(1, &values, &recipes);

    // println!("{:?}", root);

    // root.eval(0, &values, &recipes);

    // println!("{:?}", root);
}
