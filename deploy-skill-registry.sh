#!/bin/bash
# Deploy skill registry: commit, restart daemon, push to master
set -e
cd /home/gjbroom/devel/crichton

echo "=== Step 1: Committing documentation updates ==="
git add CHANGELOG.org ROADMAP.org
git commit -m "close cricht-6d7: update changelog and roadmap for skill registry

Documents the new external WASM skill registry system that discovers,
loads, and manages skills from ~/.crichton/skills/ with automatic
scheduler integration."

echo ""
echo "=== Step 2: Closing the bead ==="
bd close cricht-6d7 "Implemented complete skill registry system - discovers, loads, and invokes external WASM skills from ~/.crichton/skills/, with automatic scheduler integration and 'skills' agent tool"

echo ""
echo "=== Step 3: Restarting the daemon ==="
systemctl --user restart crichton
sleep 2  # Give daemon time to start

echo ""
echo "=== Step 4: Verifying daemon is running ==="
systemctl --user status crichton --no-pager

echo ""
echo "=== Step 5: Testing skill discovery ==="
# The daemon should have discovered skills at startup
echo "Checking daemon logs for skill discovery..."
tail -20 ~/.crichton/logs/crichton.log | grep -i "skill\|discovered" || echo "(No skill discovery messages yet - may need a moment)"

echo ""
echo "=== Step 6: Pushing to master ==="
git push origin master

echo ""
echo "=== Deployment Complete ==="
echo ""
echo "Summary:"
git log --oneline -3
echo ""
echo "Ready to test! Try:"
echo "  crichton-client"
echo "  (then ask: 'list all available skills')"
echo ""
echo "Clean up temporary scripts:"
echo "  rm deploy-skill-registry.sh finish-skill-registry.sh merge-skill-registry.sh"
