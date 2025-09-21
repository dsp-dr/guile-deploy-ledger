#!/usr/bin/env bash

# Recorded demo script for Guile Deploy Ledger
# This script will be run inside tmux and recorded with asciicinema

set +e  # Continue on errors for demo purposes

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m'

# Get script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Helper functions
type_out() {
    local text="$1"
    local delay="${2:-0.05}"
    for (( i=0; i<${#text}; i++ )); do
        echo -n "${text:$i:1}"
        sleep "$delay"
    done
    echo
}

pause() {
    sleep "${1:-2}"
}

clear_screen() {
    clear
    sleep 0.5
}

print_banner() {
    echo -e "${CYAN}╔════════════════════════════════════════════════════════════╗${NC}"
    echo -e "${CYAN}║         GUILE DEPLOY LEDGER - LIVE DEMONSTRATION          ║${NC}"
    echo -e "${CYAN}╚════════════════════════════════════════════════════════════╝${NC}"
    echo
}

# Start demo
clear_screen
print_banner

type_out "Welcome to the Guile Deploy Ledger demonstration!" 0.03
pause 1
type_out "Let's explore the deployment tracking system..." 0.03
pause 2

# Section 1: Project Overview
clear_screen
echo -e "${YELLOW}═══ PROJECT OVERVIEW ═══${NC}"
echo
type_out "$ ls -la" 0.02
ls -la | head -15
pause 3

echo
type_out "$ cat README.org | head -20" 0.02
cat README.org | head -20
pause 3

# Section 2: Running Simple Example
clear_screen
echo -e "${YELLOW}═══ SIMPLE DEPLOYMENT EXAMPLE ═══${NC}"
echo
type_out "Let's create a deployment event..." 0.03
pause 1

echo
type_out "$ guile -L src --no-auto-compile examples/simple-deployment.scm" 0.02
pause 1
guile -L src --no-auto-compile examples/simple-deployment.scm 2>/dev/null
pause 4

# Section 3: Architecture Demo Preview
clear_screen
echo -e "${YELLOW}═══ ARCHITECTURE WALKTHROUGH ═══${NC}"
echo

type_out "The system is built in layers:" 0.03
echo
echo "  1. Core Data Types (SRFI-9 records)"
echo "  2. Event Processing Pipeline"
echo "  3. Storage Abstraction"
echo "  4. Query & Analytics"
echo "  5. Reporting & Visualization"
echo "  6. Integration Points"
pause 3

echo
type_out "Each layer is modular and extensible..." 0.03
pause 2

# Section 4: Show Metrics Dashboard Structure
clear_screen
echo -e "${YELLOW}═══ METRICS DASHBOARD ═══${NC}"
echo

cat << 'EOF'
╔══════════════════════════════════════╗
║ DEPLOYMENT FREQUENCY                 ║
╟──────────────────────────────────────╢
║ Current: 12.4 deployments/day        ║
║ Target:  15.0 deployments/day        ║
║ [████████████████████░░░░░░░░░░] 82% ║
╚══════════════════════════════════════╝

Service Health Matrix:
┌─────────────────┬──────┬─────────┬──────┐
│ Service         │ Deps │ Success │ MTTR │
├─────────────────┼──────┼─────────┼──────┤
│ api-gateway     │  45  │  97.8%  │  8m  │
│ user-service    │  34  │  94.1%  │ 12m  │
│ payment-service │  12  │ 100.0%  │  5m  │
└─────────────────┴──────┴─────────┴──────┘
EOF
pause 4

# Section 5: Rollback Scenario
clear_screen
echo -e "${YELLOW}═══ ROLLBACK SCENARIO ═══${NC}"
echo

type_out "Simulating deployment failure and rollback..." 0.03
echo

echo -e "${GREEN}[10:45:00]${NC} Deployment v3.2.0 initiated"
pause 0.5
echo -e "${GREEN}[10:52:30]${NC} 70% of instances updated"
pause 0.5
echo -e "${RED}[10:53:15]${NC} Error rate spike detected!"
pause 0.5
echo -e "${YELLOW}[10:53:45]${NC} Automated rollback triggered"
pause 0.5
echo -e "${GREEN}[10:57:00]${NC} Rollback completed"
pause 0.5
echo -e "${GREEN}[10:58:00]${NC} System stability confirmed ✓"
pause 3

# Section 6: Multi-Service Dependencies
clear_screen
echo -e "${YELLOW}═══ MULTI-SERVICE ORCHESTRATION ═══${NC}"
echo

type_out "Service Dependency Graph:" 0.03
echo
cat << 'EOF'
frontend-ui (v4.0.0)
  ├─ api-gateway (v2.4.0)
  │   ├─ auth-service (v1.8.0)
  │   │   ├─ postgres-db
  │   │   └─ redis-cache
  │   ├─ user-service (v3.1.0)
  │   └─ order-service (v2.5.0)
  └─ cdn-service (v1.2.0)
EOF
pause 3

echo
type_out "Deployment order calculated by topological sort..." 0.03
pause 2

# Section 7: Show Available Demos
clear_screen
echo -e "${YELLOW}═══ AVAILABLE INTERACTIVE DEMOS ═══${NC}"
echo

echo "1. Architecture Walkthrough"
echo "   Walk through system architecture with live examples"
echo
echo "2. Metrics Dashboard"
echo "   Real-time deployment metrics and analytics"
echo
echo "3. Rollback Scenario"
echo "   Failure detection and automated rollback"
echo
echo "4. Multi-Service Orchestrator"
echo "   Coordinated deployment of interdependent services"
echo
pause 3

type_out "Run ./demos/run-demos.sh to explore interactively!" 0.03
pause 2

# Section 8: Key Features Summary
clear_screen
echo -e "${YELLOW}═══ KEY FEATURES ═══${NC}"
echo

features=(
    "✓ Immutable event sourcing"
    "✓ Multiple deployment strategies"
    "✓ Automated rollback capabilities"
    "✓ Service dependency management"
    "✓ Real-time metrics tracking"
    "✓ Complete audit trail"
    "✓ Integration with CI/CD pipelines"
    "✓ Extensible plugin system"
)

for feature in "${features[@]}"; do
    echo -e "${GREEN}${feature}${NC}"
    sleep 0.3
done
pause 2

# Final
clear_screen
print_banner
echo
echo -e "${GREEN}Thank you for watching the Guile Deploy Ledger demo!${NC}"
echo
echo "For more information:"
echo "  • Documentation: docs/"
echo "  • Examples: examples/"
echo "  • Presentations: presentations/"
echo
echo "Repository: https://github.com/org/guile-deploy-ledger"
echo
pause 3

echo -e "${CYAN}Demo recording complete.${NC}"