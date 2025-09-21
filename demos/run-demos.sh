#!/usr/bin/env bash

# Demo runner for Guile Deploy Ledger
# Provides an interactive menu to run various demos

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
MAGENTA='\033[0;35m'
CYAN='\033[0;36m'
NC='\033[0m'

# Get the script directory
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Function to print header
print_header() {
    clear
    echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo -e "${CYAN}        GUILE DEPLOY LEDGER - DEMO SUITE${NC}"
    echo -e "${CYAN}════════════════════════════════════════════════════════════${NC}"
    echo
}

# Function to run a demo
run_demo() {
    local demo_path="$1"
    local demo_name="$2"

    echo -e "${YELLOW}Starting demo: ${demo_name}${NC}"
    echo -e "${YELLOW}────────────────────────────────────────${NC}"
    echo

    if [ -f "$demo_path" ]; then
        cd "$(dirname "$demo_path")"
        guile -L "$PROJECT_ROOT/src" --no-auto-compile "$(basename "$demo_path")"
    else
        echo -e "${RED}Demo file not found: $demo_path${NC}"
    fi

    echo
    echo -e "${CYAN}Press Enter to return to menu...${NC}"
    read -r
}

# Main menu
show_menu() {
    print_header

    echo -e "${GREEN}Available Demos:${NC}"
    echo
    echo "  1) Architecture Walkthrough"
    echo "     Walk through system architecture with live examples"
    echo
    echo "  2) Metrics Dashboard"
    echo "     Interactive dashboard showing deployment metrics"
    echo
    echo "  3) Rollback Scenario"
    echo "     Demonstrates failure detection and automated rollback"
    echo
    echo "  4) Multi-Service Orchestrator"
    echo "     Coordinated deployment of interdependent services"
    echo
    echo "  5) Simple Deployment Example"
    echo "     Basic usage of deployment tracking features"
    echo
    echo "  6) View Live Demo Presentation (PDF)"
    echo "     Open the live demo presentation slides"
    echo
    echo "  q) Quit"
    echo
    echo -e "${YELLOW}Select demo (1-6 or q): ${NC}"
}

# Main loop
while true; do
    show_menu
    read -r choice

    case $choice in
        1)
            run_demo "$SCRIPT_DIR/architecture-walkthrough/demo.scm" \
                    "Architecture Walkthrough"
            ;;
        2)
            run_demo "$SCRIPT_DIR/metrics-dashboard/dashboard.scm" \
                    "Metrics Dashboard"
            ;;
        3)
            run_demo "$SCRIPT_DIR/rollback-scenario/scenario.scm" \
                    "Rollback Scenario"
            ;;
        4)
            run_demo "$SCRIPT_DIR/multi-service-deploy/orchestrator.scm" \
                    "Multi-Service Orchestrator"
            ;;
        5)
            run_demo "$PROJECT_ROOT/examples/simple-deployment.scm" \
                    "Simple Deployment Example"
            ;;
        6)
            pdf_path="$PROJECT_ROOT/presentations/live-demo/live-demo.pdf"
            if [ -f "$pdf_path" ]; then
                echo -e "${GREEN}Opening presentation PDF...${NC}"
                if command -v open >/dev/null 2>&1; then
                    open "$pdf_path"
                elif command -v xdg-open >/dev/null 2>&1; then
                    xdg-open "$pdf_path"
                else
                    echo -e "${YELLOW}Please open: $pdf_path${NC}"
                fi
            else
                echo -e "${YELLOW}Generating presentation PDF...${NC}"
                (cd "$PROJECT_ROOT/presentations/live-demo" && make pdf)
                echo -e "${GREEN}PDF generated. Press Enter to continue...${NC}"
                read -r
            fi
            ;;
        q|Q)
            echo -e "${GREEN}Thank you for exploring Guile Deploy Ledger!${NC}"
            exit 0
            ;;
        *)
            echo -e "${RED}Invalid option. Please try again.${NC}"
            sleep 2
            ;;
    esac
done