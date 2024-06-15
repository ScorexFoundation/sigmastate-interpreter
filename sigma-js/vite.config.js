import { defineConfig } from 'vitest/config';
import tsconfigPaths from 'vite-tsconfig-paths';

export default defineConfig({
  test: {
    globals: true,
    environment: 'node',
    include: ['**.spec.ts'],
  },
  plugins: [
    tsconfigPaths()
  ]
});
