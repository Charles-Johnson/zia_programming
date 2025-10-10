#[cfg(test)]
use std::time::{Duration, Instant};

pub const TUTORIALS: (Tutorial<18>, Tutorial<12>, Tutorial<12>) = (
    Tutorial {
        title: "Factorial",
        steps: [
            TutorialStep {
                command: "let 1 ! -> 1",
                explanation: "You can first define the factorial operator, !, for one",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let assoc +1 -> left",
                explanation: "and define the increment operator +1 to be left associative",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let (_x_ +1) ! -> (_x_ +1) * _x_ !",
                explanation: "Then define the factorial of the next integer after _x_, _x_ +1, for any _x_ in terms of _x_ !",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let _x_ * 1 -> _x_",
                explanation: "Need to define the multiplication operator, *. You can start by defining that anything multiplied by one is itself",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let 2 := 1 +1",
                explanation: "Define what two is in terms of the increment operator, +1",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "2 !",
                explanation: "Finally you can evaluate two factorial!",
                #[cfg(test)]
                expected_evaluation: "2"
            },
            TutorialStep {
                command: "let 3 := 2 +1",
                explanation: "Next let's define three",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "3 !",
                explanation: "Now check what three factorial is",
                #[cfg(test)]
                expected_evaluation: "3 * 2"
            },
            TutorialStep {
                command: "let * precedes +",
                explanation: "We can reduce a multiplication expression to an addition expression but first we define the relative operator precendence",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let + (precedes ->)",
                explanation: "This allows reduction rules to not need as many parentheses",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let (_x_ * (_y_ +1) -> _x_ * _y_ + _x_)",
                explanation: "Now let's define how to reduce a multiplication expression",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "3 !",
                explanation: "Now three factorial can be broken down further",
                #[cfg(test)]
                expected_evaluation: "3 + 3"
            },
            TutorialStep {
                command: "let _x_ + 1 -> (_x_ +1)",
                explanation: "Adding one to a number increments that number",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let _x_ + (_y_ +1) -> (_x_ +1) + _y_",
                explanation: "Helps to reduce addition expressions",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let 4 := 3 +1",
                explanation: "Define four",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let 5 := 4 +1",
                explanation: "Define five",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let 6 := 5 +1",
                explanation: "Define six",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "3 !",
                explanation: "Now three factorial can be broken down even further",
                #[cfg(test)]
                expected_evaluation: "6"
            },
        ]
    },
    Tutorial {
        title: "Relationships",
        steps: [
            TutorialStep {
                command: "let Alice is parent of Bob",
                explanation: "We can define Alice's relationship to Bob",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let (_x_ is parent of _y_) => _y_ is child of _x_",
                explanation: "Determine child relationship from parent relationship",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "Bob is child of Alice",
                explanation: "Let's check",
                #[cfg(test)]
                expected_evaluation: "true"
            },
            TutorialStep {
                command: "let (not _x_) => (_x_ -> false)",
                explanation: "If the negation of a predicate is true then the predicate itself is false",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let (_x_ is parent of _y_) => not _x_ is child of _y_",
                explanation: "Parents can't be children of their children",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "Alice is child of Bob",
                explanation: "Let's check",
                #[cfg(test)]
                expected_evaluation: "false"
            },
            TutorialStep {
                command: "let ((_y_ exists_such_that) (_x_ is parent of _y_) and (_y_ is parent of _z_)) => (_x_ is grandparent of _z_)",
                explanation: "Define grandparent is terms of parent relationships",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let Charlie is parent of Alice",
                explanation: "This should make Charlie the grandparent of Bob",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "Charlie is grandparent of Bob",
                explanation: "Let's check",
                #[cfg(test)]
                expected_evaluation: "true"
            },
            TutorialStep {
                command: "let ((_y_ exists_such_that) (_y_ is parent of _x_) and (_y_ is parent of _z_)) => (_x_ is sibling of _z_)",
                explanation: "Define sibling is terms of parent relationships",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let Alice is parent of Eve",
                explanation: "This should mean that Eve is sibling of Bob",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "Eve is sibling of Bob",
                explanation: "Let's check",
                #[cfg(test)]
                expected_evaluation: "true"
            }
        ]
    },
    Tutorial {
        title: "Arrays",
        steps: [
            TutorialStep {
                command: "let ([ _x_ ])[ 0 ] -> _x_",
                #[cfg(test)]
                expected_evaluation: "",
                explanation: "Define the base case for array access",
            },
            TutorialStep {
                command: "([ 2 ])[ 0 ]",
                #[cfg(test)]
                expected_evaluation: "2",
                explanation: "We can now access the element of any array with that has a single element"
            },
            TutorialStep {
                command: "let ([ _x_ , _y_)[ 0 ] -> _x_",
                #[cfg(test)]
                expected_evaluation: "",
                explanation: "Define the first element of an array with more than one element"
            },
            TutorialStep {
                command: "([ 5 , 3 ])[ 0 ]",
                #[cfg(test)]
                expected_evaluation: "5",
                explanation: "We can access the first element of any array"
            },
            TutorialStep {
                command: "let ([ _x_ , _y_)[ (_i_ +1) ] -> ([ _y_)[ _i_ ]",
                #[cfg(test)]
                expected_evaluation: "",
                explanation: "Define the how to access subsequent elements of an array"
            },
            TutorialStep {
                command: "let 1 := 0 +1",
                #[cfg(test)]
                expected_evaluation: "",
                explanation: "Define the number one"
            },
            TutorialStep {
                command: "([ 5 , 3 ])[ 1 ]",
                #[cfg(test)]
                expected_evaluation: "3",
                explanation: "We can now access any element of any array"
            },
            TutorialStep {
                command: "let (_x_ ]) push _y_ -> _x_ , _y_ ]",
                #[cfg(test)]
                expected_evaluation: "",
                explanation: "Define pushing an element to the end of an array"
            },
            TutorialStep {
                command: "let ([ _x_) push _y_ -> [ _x_ push _y_",
                #[cfg(test)]
                expected_evaluation: "",
                explanation: "Define pushing an element to the start of an array"
            },
            TutorialStep {
                command: "([ 5 ]) push 3",
                #[cfg(test)]
                expected_evaluation: "[ 5 , 3 ]",
                explanation: "We can now push to arrays with a single element"
            },
            TutorialStep {
                command: "let (_x_ , _y_) push _z_ -> _x_ , _y_ push _z_",
                #[cfg(test)]
                expected_evaluation: "",
                explanation: "Define pushing an element to the middle of an array"
            },
            TutorialStep {
                command: "([ 5 , 3 ]) push 1",
                #[cfg(test)]
                expected_evaluation: "[ 5 , 3 , 1 ]",
                explanation: "We can now push to arrays with multiple elements"
            },
        ]
    }
);

pub struct Model {
    pub steps: &'static [TutorialStep],
    pub current_step_index: usize,
    pub showing_evaluation: bool,
}

pub struct Tutorial<const N: usize> {
    pub title: &'static str,
    pub steps: [TutorialStep; N],
}

pub struct TutorialStep {
    pub command: &'static str,
    pub explanation: &'static str,
    #[cfg(test)]
    expected_evaluation: &'static str,
}

#[cfg(test)]
impl TutorialStep {
    fn test(&self, context: &mut zia::multi_threaded::Context) {
        let i = Instant::now();
        assert_eq!(
            context.execute(self.command),
            self.expected_evaluation,
            "Failed at {0}",
            self.command
        );
        let time_taken = i.elapsed();
        let limit_in_seconds = 15;
        assert!(time_taken < Duration::from_secs(limit_in_seconds), "{0} took longer than {limit_in_seconds} second - it took {time_taken:?}", self.command);
    }
}

#[cfg(test)]
mod test {

    use zia::multi_threaded::NEW_CONTEXT;

    use super::TUTORIALS;

    #[test]
    fn factorial_tutorial() {
        let mut context = NEW_CONTEXT.clone();
        for step in TUTORIALS.0.steps {
            step.test(&mut context);
        }
    }
    #[test]
    fn relationships_tutorial() {
        let mut context = NEW_CONTEXT.clone();
        for step in TUTORIALS.1.steps {
            step.test(&mut context);
        }
    }
    #[test]
    fn array_tutorial() {
        let mut context = NEW_CONTEXT.clone();
        for step in TUTORIALS.2.steps {
            step.test(&mut context);
        }
    }
}
