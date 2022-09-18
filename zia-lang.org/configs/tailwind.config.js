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

const colors = {
  primary: "#0B723B",
  secondary: "#FFFFFF",
  variable_concept: "#FFBF00",
  abstract_concept: "#BF00FF",
  concrete_concept: "#4000FF",
  new_concept: "#80FF00",
  unmatched_parenthesis: "#FF0000"
};

const spacing = {
  2: '0.5rem',
  5: '1.25rem',
  6: '1.5rem',
  8: '2rem',
  10: '2.5rem',
  16: '4rem',
  20: '5rem',
  24: '6rem',
};

const theme = {
  spacing,
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
  colors,
  borderRadius: {
    '28px': '28px'
  },
  columns: {},
  animation: {},
  aspectRatio: {},
  backgroundImage: {},
  backgroundPsotion: {},
  backgroundSize: {},
  blur: {},
  brightness: {},
  borderColor: {
    primary: colors.primary,
  },
  borderOpacity: {},
  borderSpacing: {},
  borderWidth: {
    2: spacing[2],
  },
  boxShadow: {},
  boxShadowColor: {},
  caretColor: {},
  accentColor: {},
  contrast: {},
  content: {},
  cursor: {},
  divideColor: {},
  divideWidth: {},
  dropShadow: {},
  fill: {},
  grayscale: {},
  hueRotate: {},
  invert: {},
  flex: {
    1: '1 1 0%',
  },
  flexBasis: {},
  flexGrow: {
    DEFAULT: '1',
  },
  flexShrink: {},
  fontWeight: {
    thin: '100',
  },
  gap: {},
  gradientColorStops: {},
  gridAutoColumns: {},
  gridAutoRows: {},
  gridColumn: {},
  gridColumnEnd: {},
  gridColumnStart: {},
  gridRow: {},
  gridRowStart: {},
  gridRowEnd: {},
  gridTemplateColumns: {},
  gridTemplateRows: {},
  height: { 10: spacing[10], screen: "100vh" },
  inset: {},
  keyframes: {},
  letterSpacing: {},
  lineHeight: {},
  listStyleType: {},
  maxHeight: {},
  maxWidth: {},
  minHeight: {},
  minWidth: {},
  objectPosition: {},
  opacity: {},
  order: {},
  padding: { 2: spacing[2] },
  placeholderColor: {},
  placeholderOpacity: {},
  outlineColor: {},
  outlineOffset: {},
  outlineWidth: {},
  ringColor: {},
  ringOffsetColor: {},
  ringOffsetWidth: {},
  ringOpacity: {},
  ringWidth: {},
  rotate: {},
  saturate: {},
  scale: {},
  scrollMargin: {},
  scrollPadding: {},
  sepia: {},
  skew: {},
  space: {},
  stroke: {},
  strokeWidth: {},
  textDecorationColor: {},
  textDecorationThickness: {},
  textUnderlineOffset: {},
  textIndent: {},
  textOpacity: {},
  transformOrigin: {},
  transitionDelay: {},
  transitionDuration: {},
  transitionProperty: {},
  transitionTimingFunction: {},
  translate: {},
  width: { ...spacing },
  willChange: {},
  zIndex: {}
};

module.exports = {
  theme,
  variants: {},
  plugins: []
}
