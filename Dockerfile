# The same builder is used locally and by GitHub Actions.
FROM node:24-bookworm-slim AS dependencies
WORKDIR /app
ENV ASTRO_TELEMETRY_DISABLED=1
RUN npm install --global pnpm@11.3.0
COPY package.json pnpm-lock.yaml pnpm-workspace.yaml ./
RUN pnpm install --frozen-lockfile

FROM dependencies AS development
COPY astro.config.ts astro-paper.config.ts tsconfig.json ./
COPY src/ src/
COPY public/ public/
EXPOSE 4321
CMD ["pnpm", "exec", "astro", "dev", "--host", "0.0.0.0", "--port", "4321"]

FROM development AS build
COPY scripts/ scripts/
COPY tests/ tests/
RUN pnpm test && pnpm build

FROM scratch AS export
COPY --from=build /app/dist/ /
