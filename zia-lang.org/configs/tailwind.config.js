// [...range(1,5,2)] => [1,3,5]
function* range(start, end, step) {
  if (start > end) return;
  yield start;
  yield* range(start + step, end, step);
};

// For NodeJS < 12
Object.fromEntries = Object.fromEntries || ((iterable) => {
  return [...iterable].reduce((obj, [key, val]) => {
    obj[key] = val
    return obj
  }, {})
});

module.exports = {
  theme: {
    spacing: {
      2: "2px",
      5: "5px",
      6: "6px",
      8: "8px",
      10: "10px",
      16: "16px",
      20: "20px",
      24: "24px"
    },
    screens: {
      'sm': '569px',
      // => @media (min-width: 569px) { ... }

      'lg': '1025px',
      // => @media (min-width: 1025px) { ... }
    },
    fontFamily: {
      display: ['Metropolis', 'sans-serif'],
      monospace: ['Courier New', 'monospace'],
    },
    fontSize: { 29: "29px", 45: "45px", 55: "55px" },
    colors: {
      primary: "#0B723B",
      secondary: "#FFFFFF",
      variable_concept: "#FFBF00",
      abstract_concept: "#BF00FF",
      concrete_concept: "#4000FF",
      new_concept: "#80FF00",
      unmatched_parenthesis: "#FF0000"
    },
    borderRadius: {
      '28px': '28px'
    }
  },
  variants: {},
  plugins: []
}
