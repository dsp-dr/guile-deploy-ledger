# Demos and Presentations

## Created Presentations

### 1. Live Demo Presentation (`presentations/live-demo/`)
- **Format**: Org-mode/Beamer → PDF
- **PDF Size**: 231KB
- **Contents**:
  - Blue-green deployment demo
  - Canary deployment with progressive rollout
  - Emergency rollback procedures
  - Multi-service coordinated deployment
  - Metrics and analytics dashboard
  - Live coding session examples
  - Integration with Slack, Prometheus
- **Usage**: `cd presentations/live-demo && gmake pdf`

### 2. Technical Deep Dive (`presentations/technical-deep-dive/`)
- **Format**: Org-mode/Beamer → PDF
- **PDF Size**: 175KB
- **Contents**:
  - System architecture overview
  - Implementation details
  - Performance benchmarks
  - Database schema
  - API design
- **Generated**: ✓

### 3. Executive Overview (`presentations/executive-overview/`)
- **Format**: Org-mode/Beamer → PDF
- **PDF Size**: 234KB
- **Contents**:
  - Business value proposition
  - ROI metrics
  - Risk reduction
  - Compliance benefits
- **Generated**: ✓

### 4. Introduction to Deploy Ledger (`presentations/intro-to-deploy-ledger/`)
- **Format**: Org-mode/Beamer → PDF
- **PDF Size**: 196KB
- **Contents**:
  - Basic concepts
  - Getting started guide
  - Use cases
  - Quick wins
- **Generated**: ✓

## Interactive Demos

### 1. Architecture Walkthrough (`demos/architecture-walkthrough/demo.scm`)
Interactive tour through the system architecture with live examples:
- Core data types demonstration
- Event processing pipeline
- Storage abstraction layer
- Query & analytics capabilities
- Reporting & visualization
- Integration points

**Run**: `guile -L src demos/architecture-walkthrough/demo.scm`

### 2. Metrics Dashboard (`demos/metrics-dashboard/dashboard.scm`)
Real-time metrics dashboard showing:
- KPI cards (deployment frequency, lead time, MTTR, failure rate)
- Deployment trend graphs
- Service health matrix
- Recent deployments feed
- Active alerts panel
- 24-hour statistics

**Run**: `guile -L src demos/metrics-dashboard/dashboard.scm`

### 3. Rollback Scenario (`demos/rollback-scenario/scenario.scm`)
Step-by-step demonstration of failure and rollback:
- Phase 1: Deployment initiation
- Phase 2: Rolling update progress
- Phase 3: Problem detection
- Phase 4: Rollback decision matrix
- Phase 5: Rollback execution
- Phase 6: Recovery verification
- Phase 7: Post-mortem analysis

**Run**: `guile -L src demos/rollback-scenario/scenario.scm`

### 4. Multi-Service Orchestrator (`demos/multi-service-deploy/orchestrator.scm`)
Demonstrates coordinated deployment of 10 interdependent services:
- Service dependency graph visualization
- Topological sort for deployment order
- Health check validation
- Cascade rollback on failure
- Deployment timeline and statistics

**Run**: `guile -L src demos/multi-service-deploy/orchestrator.scm`

### 5. Simple Deployment Example (`examples/simple-deployment.scm`)
Basic usage demonstration:
- Creating deployment events
- Service metadata management
- Rollback event creation
- Data structure exploration

**Run**: `guile -L src examples/simple-deployment.scm`

## Demo Runner

An interactive menu system is available to run all demos:

```bash
./demos/run-demos.sh
```

Features:
- Interactive menu with descriptions
- Automatic environment setup
- Color-coded output
- PDF presentation viewer integration

## Key Demo Features

### Visual Elements
- Color-coded status indicators (✓ success, ✗ failure, ⚠ warning)
- ASCII progress bars and graphs
- Box-drawing characters for tables and borders
- Real-time status updates

### Interactive Elements
- Step-by-step progression with pauses
- User confirmation prompts
- Simulated delays for realism
- Clear section headers and transitions

### Educational Value
- Shows real-world scenarios
- Demonstrates error handling
- Illustrates rollback procedures
- Explains architectural decisions
- Provides actionable insights

## Running Demos

### Prerequisites
```bash
# Ensure Guile is installed
guile --version

# Set up load path
export GUILE_LOAD_PATH="./src:$GUILE_LOAD_PATH"
```

### Quick Start
```bash
# Run the demo suite
./demos/run-demos.sh

# Or run individual demos
guile -L src demos/architecture-walkthrough/demo.scm
guile -L src demos/metrics-dashboard/dashboard.scm
guile -L src demos/rollback-scenario/scenario.scm
guile -L src demos/multi-service-deploy/orchestrator.scm
```

### Generating Presentations
```bash
# Generate all presentations
for dir in presentations/*/; do
  (cd "$dir" && gmake pdf)
done

# View a presentation
open presentations/live-demo/live-demo.pdf
```

## Demo Customization

All demos can be customized by modifying:
- Service names and versions
- Deployment strategies
- Failure thresholds
- Timing delays
- Visual formatting

The demos use the actual `deploy-ledger` core types, ensuring consistency with the production system.

## Presentation Formats

All presentations are:
- Written in Org-mode for easy editing
- Exported to LaTeX/Beamer for professional formatting
- Generated as PDFs for universal viewing
- Styled with consistent themes
- Include code examples and diagrams

## Next Steps

To extend the demo suite:
1. Add industry-specific scenarios
2. Create video recordings
3. Build web-based interactive demos
4. Add performance benchmarking demos
5. Include security audit demonstrations