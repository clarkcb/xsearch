module.exports = {
  env: {
    es2021: true,
    jest: true,
    node: true
  },
  extends: 'eslint:recommended',
  parser: '@babel/eslint-parser',
  parserOptions: {
    ecmaVersion: 12,
    sourceType: 'script', // the default; should it be 'module'?
    ecmaFeatures: {
      impliedString: true // enable global strict mode (if ecmaVersion is 5 or greater)
    }
  },
  rules: {
  }
}
