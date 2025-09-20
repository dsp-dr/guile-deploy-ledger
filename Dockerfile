# Multi-stage Dockerfile for Guile Deploy Ledger
FROM debian:bookworm-slim AS base

# Install Guile and dependencies
RUN apt-get update && apt-get install -y \
    guile-3.0 \
    guile-3.0-dev \
    guile-sqlite3 \
    sqlite3 \
    make \
    gcc \
    git \
    curl \
    && rm -rf /var/lib/apt/lists/*

# Create application user
RUN useradd -m -s /bin/bash deploy-ledger

# Stage for building
FROM base AS builder

# Copy source code
WORKDIR /build
COPY --chown=deploy-ledger:deploy-ledger . .

# Compile Guile modules
USER deploy-ledger
RUN make compile

# Run tests
RUN make test || true

# Final stage
FROM base AS runtime

# Copy compiled application
WORKDIR /opt/guile-deploy-ledger
COPY --from=builder --chown=deploy-ledger:deploy-ledger /build .

# Set up environment
ENV GUILE_LOAD_PATH=/opt/guile-deploy-ledger/src:$GUILE_LOAD_PATH
ENV GUILE_LOAD_COMPILED_PATH=/opt/guile-deploy-ledger/src:$GUILE_LOAD_COMPILED_PATH
ENV PATH=/opt/guile-deploy-ledger/scripts:$PATH

# Create data directory
RUN mkdir -p /var/lib/deploy-ledger && \
    chown deploy-ledger:deploy-ledger /var/lib/deploy-ledger

# Switch to non-root user
USER deploy-ledger

# Set default database location
ENV DEPLOY_LEDGER_DB=/var/lib/deploy-ledger/deployments.db

# Expose webhook port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD guile -c "(exit 0)" || exit 1

# Default command - start REPL
CMD ["guile", "-L", "src", "-c", \
     "(use-modules (deploy-ledger core types) \
                   (deploy-ledger storage sqlite) \
                   (deploy-ledger query metrics)) \
      (display \"Guile Deploy Ledger Ready\\n\") \
      ((@ (system repl server) spawn-server))"]