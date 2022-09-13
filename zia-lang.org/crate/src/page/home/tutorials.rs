pub const TUTORIALS: (Tutorial<18>, Tutorial<0>) = (
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
                command: "let (prec *) > prec +",
                explanation: "We can reduce a multiplication expression to an addition expression but first we define the relative operator precendence",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let (prec +) > (prec ->)",
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
        steps: []
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
mod test {
    use zia::multi_threaded::NEW_CONTEXT;

    use super::TUTORIALS;

    #[test]
    fn factorial_tutorial() {
        let mut context = NEW_CONTEXT.clone();
        for step in TUTORIALS.0.steps {
            assert_eq!(
                context.execute(step.command),
                step.expected_evaluation,
                "Failed at {0}",
                step.command
            );
        }
    }
}
