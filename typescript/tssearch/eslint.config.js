import tsParser from '@typescript-eslint/parser';
import tsPlugin from '@typescript-eslint/eslint-plugin';
import prettierPlugin from 'eslint-plugin-prettier';
import prettierConfig from 'eslint-config-prettier';

export default [
  {
    // Apply to all TypeScript and JavaScript files
    files: ['**/*.{ts,tsx,js,jsx}'],
    languageOptions: {
      parser: tsParser,
      ecmaVersion: 'latest',
      sourceType: 'module',
    },
    plugins: {
      '@typescript-eslint': tsPlugin,
      prettier: prettierPlugin,
    },
    rules: {
      // Include recommended TypeScript rules
      ...tsPlugin.configs.recommended.rules,
      // Run Prettier as an ESLint rule and report differences as errors
      'prettier/prettier': 'error',
      // Custom overrides
      '@typescript-eslint/no-unused-vars': ['error', { argsIgnorePattern: '^_' }],
      'no-console': 'warn',
    },
  },
  // Turns off all ESLint rules that conflict with Prettier
  prettierConfig,
  {
    // Ignore build folders and configuration files
    ignores: ['dist/', 'node_modules/', 'eslint.config.js'],
  },
];
