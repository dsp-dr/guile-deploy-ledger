# Demo Recordings

## Terminal Recordings with asciicinema & agg

### Full Demo Recording
- **File**: `deploy-ledger-demo.cast` (20KB) / `deploy-ledger-demo.gif` (506KB)
- **Duration**: ~45 seconds
- **Created with**: tmux + asciicinema + agg
- **Contents**:
  - Project overview
  - Simple deployment example
  - Architecture walkthrough preview
  - Metrics dashboard visualization
  - Rollback scenario demonstration
  - Multi-service orchestration
  - Available interactive demos
  - Key features summary

### Quick Demo Recording
- **File**: `quick-demo.cast` (2.4KB) / `quick-demo.gif` (73KB)
- **Duration**: ~15 seconds
- **Created with**: asciicinema + agg
- **Contents**:
  - Simple deployment example
  - Core functionality demonstration

## Recording Setup Used

### 1. tmux Session
```bash
# Create tmux session with specific dimensions
tmux new-session -d -s demo-recording -x 100 -y 30

# Send commands to session
tmux send-keys -t demo-recording "cd /path/to/project" C-m
```

### 2. asciicinema Recording
```bash
# Record in tmux session
asciinema rec -c './demos/recorded-demo.sh' demos/deploy-ledger-demo.cast

# Or record directly
asciinema rec -c './demos/quick-demo.sh' demos/quick-demo.cast
```

### 3. Convert to GIF with agg
```bash
# Convert with Monokai theme
agg deploy-ledger-demo.cast deploy-ledger-demo.gif \
    --theme monokai \
    --font-size 14 \
    --speed 1.5

# Quick demo with larger font
agg quick-demo.cast quick-demo.gif \
    --theme monokai \
    --font-size 16 \
    --speed 1.2
```

## Viewing Recordings

### Play asciicinema Recording
```bash
# Play in terminal
asciinema play demos/deploy-ledger-demo.cast

# Play with speed adjustment
asciinema play -s 2 demos/deploy-ledger-demo.cast
```

### Upload to asciinema.org
```bash
# Upload for sharing (requires account)
asciinema upload demos/deploy-ledger-demo.cast
```

### View GIF Files
The GIF files can be:
- Embedded in README files
- Shared in documentation
- Used in presentations
- Posted to social media

## Demo Scripts

### Main Recording Script (`recorded-demo.sh`)
- Automated typing effect for natural appearance
- Color-coded output for clarity
- Section breaks with clear headers
- Timed pauses for readability

### Quick Demo Script (`quick-demo.sh`)
- Minimal, focused demonstration
- Shows core functionality only
- Ideal for README embedding

### Interactive Demo Runner (`run-demos.sh`)
- Menu-driven interface
- Launches all available demos
- Includes presentation viewer

## Recording Features

### Visual Elements
- **Colors**: ANSI color codes for terminal output
- **Boxes**: ASCII art for UI elements
- **Progress Bars**: Visual deployment progress
- **Status Icons**: ✓ ✗ ⚠ ℹ for different states

### Timing
- Simulated typing with variable speed
- Strategic pauses between sections
- Natural command execution delays

## Customization

### Modify Recording Theme
```bash
# Available themes: asciinema, dracula, monokai, solarized-dark, solarized-light
agg input.cast output.gif --theme dracula
```

### Adjust Speed
```bash
# Speed up 2x
agg input.cast output.gif --speed 2

# Slow down to 0.5x
agg input.cast output.gif --speed 0.5
```

### Change Dimensions
```bash
# Set specific width/height
agg input.cast output.gif --cols 120 --rows 40
```

## File Sizes

| Recording | asciicinema | GIF | Compression |
|-----------|-------------|-----|-------------|
| Full Demo | 20 KB | 506 KB | 25x |
| Quick Demo | 2.4 KB | 73 KB | 30x |

The asciicinema format is much more efficient for storage and can be played back at different speeds, while GIFs are more universally viewable.

## Sharing Options

1. **GitHub README**: Embed GIF directly
   ```markdown
   ![Demo](demos/deploy-ledger-demo.gif)
   ```

2. **Documentation**: Link to asciicinema.org
   ```markdown
   [![asciicast](https://asciinema.org/a/[id].svg)](https://asciinema.org/a/[id])
   ```

3. **Presentations**: Use GIF in slides

4. **Social Media**: Share GIF directly

## Next Steps

1. Create demo recordings for specific features
2. Record troubleshooting scenarios
3. Create installation walkthrough
4. Record performance benchmarking session
5. Create comparison demos with other tools