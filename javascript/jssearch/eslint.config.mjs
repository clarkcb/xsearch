import js from '@eslint/js';
import globals from 'globals';
import { defineConfig } from 'eslint/config';
import eslintConfigPrettier from 'eslint-config-prettier';
import jestPlugin from 'eslint-plugin-jest'; // Import the Jest plugin

export default defineConfig([
  {
    files: ['**/*.{js,mjs,cjs}'],
    plugins: { js },
    extends: ['js/recommended'],
    languageOptions: { globals: globals.node },
  },
  { files: ['**/*.js'], languageOptions: { sourceType: 'commonjs' } },
  // Restrict Jest rules and globals strictly to test files
  {
    files: ['**/*.test.js', '**/*.spec.js', '**/__tests__/**/*.js'],
    languageOptions: {
      globals: {
        ...globals.jest, // Adds global variables like describe, it, expect
      },
    },
  },

  // Spread the built-in flat recommended configuration
  ...jestPlugin.configs['flat/recommended'],
  eslintConfigPrettier, // Must be the last item!
]);
