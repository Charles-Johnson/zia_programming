pub const TUTORIALS: (Tutorial<5>, Tutorial<0>) = (
    Tutorial {
        title: "Factorial",
        steps: [
            TutorialStep {
                command: "let 1 ! -> 1",
                explanation: "You can first define the factorial operator, !, for one"
            },
            TutorialStep {
                command: "let (_x_ +1) ! -> (_x_ +1) * _x_ !",
                explanation: "Then define the factorial of the next integer after _x_, _x_ +1, for any _x_ in terms of _x_ !"
            },
            TutorialStep {
                command: "let 1 * _x_ -> _x_",
                explanation: "Need to define the multiplication operator, *. You can start by defining that anything multiplied by one is itself"
            },
            TutorialStep {
                command: "let 2 := 1 +1",
                explanation: "Define what two is in terms of the increment operator, +1"
            },
            TutorialStep {
                command: "2 !",
                explanation: "Finally you can evaluate two factorial!"
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
    pub showing_evaluation: bool
}

pub struct Tutorial<const N: usize> {
    pub title: &'static str,
    pub steps: [TutorialStep; N]
}

pub struct TutorialStep {
    pub command: &'static str,
    pub explanation: &'static str,
}
