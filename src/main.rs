use std::cell::RefCell;
use std::rc::Rc;
use std::rc::Weak;

fn main() {
    println!("Hello, binary tree!");
    let node = BinaryTreeNode::new(3);

    let another_node = BinaryTreeNode::new(5);
    let yet_another_node = BinaryTreeNode::new(7);

    node.set_child(another_node.clone(), ChildDirection::Left);
    node.set_child(yet_another_node, ChildDirection::Right);

    let another_node_2 = BinaryTreeNode::new(9);
    let yet_another_node_2 = BinaryTreeNode::new(11);

    another_node.set_child(another_node_2, ChildDirection::Left);
    another_node.set_child(yet_another_node_2.clone(), ChildDirection::Right);

    println!("{}", node);

    println!("Check parent function:");
    let parent_of_another_node = another_node.parent();
    println!("{}", *parent_of_another_node.unwrap().value().borrow());

    println!("Check changed value:");
    node.set_value(1000000);
    println!("{}", *node.value().borrow());

    //println!("Cycled tree:");

    //next line panics
    //yet_another_node_2.set_child(node.clone(), ChildDirection::Left);
}

enum ChildDirection {
    Left,
    Right,
}

struct BinaryTreeNode<T> {
    inner_node: Rc<BinaryTreeNodeInner<T>>,
}

struct BinaryTreeNodeInner<T> {
    value: RefCell<T>,
    parent: RefCell<Weak<BinaryTreeNodeInner<T>>>,
    left_child: RefCell<Option<BinaryTreeNode<T>>>,
    right_child: RefCell<Option<BinaryTreeNode<T>>>,
}

impl<T> Clone for BinaryTreeNode<T> {
    fn clone(&self) -> Self {
        BinaryTreeNode {
            inner_node: Rc::clone(&self.inner_node),
        }
    }
}

impl<T> BinaryTreeNode<T> {
    fn new(value: T) -> BinaryTreeNode<T> {
        let inner = BinaryTreeNodeInner {
            value: RefCell::new(value),
            parent: RefCell::new(Weak::new()),
            left_child: RefCell::new(None),
            right_child: RefCell::new(None),
        };
        BinaryTreeNode {
            inner_node: Rc::new(inner),
        }
    }

    fn value(&self) -> &RefCell<T> {
        &self.inner_node.value
    }

    fn set_value(&self, v: T) {
        *self.inner_node.value.borrow_mut() = v;
    }

    fn parent(&self) -> Option<BinaryTreeNode<T>> {
        let parent_rc = self.inner_node.parent.borrow().upgrade()?;
        Some(BinaryTreeNode {
            inner_node: parent_rc,
        })
    }

    fn child(&self, direction: ChildDirection) -> Option<BinaryTreeNode<T>> {
        let child = match direction {
            ChildDirection::Left => &self.inner_node.left_child,
            ChildDirection::Right => &self.inner_node.right_child,
        };
        child.borrow().clone()
    }

    fn has_another_node_in_parents(&self, another_node: &BinaryTreeNode<T>) -> bool {
        if Rc::ptr_eq(&self.inner_node, &another_node.inner_node) {
            return true;
        }
        match self.parent() {
            None => false,
            Some(p) => p.has_another_node_in_parents(another_node),
        }
    }

    fn remove_child(&self, child_to_remove: &BinaryTreeNode<T>) {
        for child in [&self.inner_node.left_child, &self.inner_node.right_child] {
            let child = &mut *child.borrow_mut();
            if child.as_ref() == Some(child_to_remove) {
				*child = None;
			}
        }
    }

    fn set_child(&self, another_node: BinaryTreeNode<T>, direction: ChildDirection) {
        if self.has_another_node_in_parents(&another_node) {
            panic!("You cannot make cycled trees!");
        }        
        let old_parent_of_new_child = another_node.parent();
        match old_parent_of_new_child {
            None => {}
            Some(unwrapped_old_parent_of_new_child) => {
                unwrapped_old_parent_of_new_child.remove_child(&another_node);
            }
        }

        *another_node.inner_node.parent.borrow_mut() = Rc::downgrade(&self.inner_node);
        let existing_child = match direction {
            ChildDirection::Left => &self.inner_node.left_child,
            ChildDirection::Right => &self.inner_node.right_child,
        };
        if let Some(child) = &*existing_child.borrow_mut() {
			*child.inner_node.parent.borrow_mut() = Weak::new();
		}
        *existing_child.borrow_mut() = Some(another_node);
    }
}

impl<T: std::fmt::Display> std::fmt::Display for BinaryTreeNode<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print(0, f)?;
        Ok(())
    }
}

impl<T: std::fmt::Display> BinaryTreeNode<T> {
    fn print(&self, indent: i32, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.print_single_node(indent, f)?;
        for direction in [ChildDirection::Left, ChildDirection::Right] {
			if let Some(child) = self.child(direction) {
				child.print(indent + 2, f)?;
			}
        }
        Ok(())
    }

    fn print_single_node(&self, indent: i32, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for _i in 0..indent {
            write!(f, " ")?;
        }
        writeln!(f, "{}", *self.value().borrow())?;
        Ok(())
    }
}

impl<T> PartialEq for BinaryTreeNode<T> {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.inner_node, &other.inner_node)
    }
}
impl<T> Eq for BinaryTreeNode<T> {}

#[test]
fn creation_test() {
    let node = BinaryTreeNode::new(18);
    assert_eq!(node.inner_node.value, RefCell::new(18));
}

#[test]
fn get_value_test() {
    let node = BinaryTreeNode::new(11);
    assert_eq!(*(node.value()), RefCell::new(11));
}

#[test]
fn clone_test() {
    let node = BinaryTreeNode::new(15);
    let another_node = node.clone();
    node.set_value(13);
    assert_eq!(*(another_node.value()), RefCell::new(13));
}

#[test]
fn get_parent_and_children_test() {
    let node = BinaryTreeNode::new(3);
    assert!(node.parent().is_none());

    let another_node = BinaryTreeNode::new(5);
    let yet_another_node = BinaryTreeNode::new(7);

    node.set_child(another_node.clone(), ChildDirection::Left);
    node.set_child(yet_another_node.clone(), ChildDirection::Right);

    assert_eq!(*(another_node.parent().unwrap().value()), RefCell::new(3));
    assert_eq!(
        *(yet_another_node.parent().unwrap().value()),
        RefCell::new(3)
    );

    assert_eq!(
        *(node.child(ChildDirection::Left).unwrap().value()),
        RefCell::new(5)
    );
    assert_eq!(
        *(node.child(ChildDirection::Right).unwrap().value()),
        RefCell::new(7)
    );
}

#[test]
fn test_for_str_values() {
    let node = BinaryTreeNode::new("three");
    assert!(node.parent().is_none());

    let another_node = BinaryTreeNode::new("five");
    let yet_another_node = BinaryTreeNode::new("seven");

    node.set_child(another_node.clone(), ChildDirection::Left);
    node.set_child(yet_another_node.clone(), ChildDirection::Right);

    assert!(another_node.parent().unwrap() == node);
    assert!(yet_another_node.parent().unwrap() == node);

    assert!(node.child(ChildDirection::Left).unwrap() == another_node);
    assert!(node.child(ChildDirection::Right).unwrap() == yet_another_node);
}

#[test]
fn test_print_node() {
    let node = BinaryTreeNode::new("three");
    println!("{}", node);
}
