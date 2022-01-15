pub const TUTORIALS: (Tutorial<5>, Tutorial<0>) = (
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
                command: "let (_x_ +1) ! -> (_x_ +1) * _x_ !",
                explanation: "Then define the factorial of the next integer after _x_, _x_ +1, for any _x_ in terms of _x_ !",
                #[cfg(test)]
                expected_evaluation: ""
            },
            TutorialStep {
                command: "let 1 * _x_ -> _x_",
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
            }
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
    expected_evaluation: &'static str
}

#[cfg(test)]
mod test {
    use zia::multi_threaded::NEW_CONTEXT;

    use super::TUTORIALS;

    #[test]
    fn factorial_tutorial() {
        let mut context = NEW_CONTEXT.clone();
        for step in TUTORIALS.0.steps {
            assert_eq!(context.execute(step.command), step.expected_evaluation);
        }
    }
}