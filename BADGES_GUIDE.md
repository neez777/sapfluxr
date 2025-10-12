# GitHub Badges Guide for sapFluxR

## What Are Badges?

Badges are small images that show the status of your project. They appear at the top of your README and give users quick information about your package.

## Current Badges (Working Immediately)

Your README now has these badges that work right away:

```markdown
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
```

These show:
- **Lifecycle: experimental** - Package is in early development (change to "maturing" or "stable" later)
- **License: GPL-3** - Your package license
- **Project Status: Active** - You're actively developing it

## Why the Old Badges Didn't Work

The original badges required services that aren't set up yet:

1. **R-CMD-check** - Requires GitHub Actions to be configured
2. **Codecov** - Requires Codecov.io account and integration
3. **CRAN status** - Only works when package is on CRAN

## Optional: Add More Simple Badges

### Add After GitHub Upload

Once you upload to GitHub, you can add:

```markdown
[![GitHub issues](https://img.shields.io/github/issues/neez777/sapFluxR)](https://github.com/neez777/sapFluxR/issues)
[![GitHub stars](https://img.shields.io/github/stars/neez777/sapFluxR)](https://github.com/neez777/sapFluxR/stargazers)
[![GitHub forks](https://img.shields.io/github/forks/neez777/sapFluxR)](https://github.com/neez777/sapFluxR/network)
```

### Version Badge

Add a version badge that you manually update:

```markdown
[![Version](https://img.shields.io/badge/version-0.2.0-orange.svg)](https://github.com/neez777/sapFluxR)
```

## Setting Up R-CMD-check (Optional - For Later)

If you want the R-CMD-check badge (recommended for serious packages):

### Step 1: Install usethis

```r
install.packages("usethis")
```

### Step 2: Set Up GitHub Actions

```r
usethis::use_github_action("check-standard")
```

This creates `.github/workflows/R-CMD-check.yaml` in your package.

### Step 3: Commit and Push

```bash
git add .github/workflows/R-CMD-check.yaml
git commit -m "Add GitHub Actions for R CMD check"
git push
```

### Step 4: Add Badge to README

After GitHub Actions runs successfully, add:

```markdown
[![R-CMD-check](https://github.com/neez777/sapFluxR/workflows/R-CMD-check/badge.svg)](https://github.com/neez777/sapFluxR/actions)
```

### What It Does

- Automatically runs `R CMD check` on your package
- Tests on multiple R versions (release, devel, oldrel)
- Tests on multiple OS (Windows, macOS, Ubuntu)
- Shows green badge if all tests pass, red if they fail

## Setting Up Codecov (Optional - For Later)

Codecov shows what percentage of your code is covered by tests.

### Step 1: Sign Up

1. Go to [codecov.io](https://codecov.io)
2. Sign in with your GitHub account
3. Enable Codecov for your sapFluxR repository

### Step 2: Add Coverage to Your Package

```r
# Install covr
install.packages("covr")

# Set up coverage
usethis::use_coverage()
usethis::use_github_action("test-coverage")
```

### Step 3: Add Badge

```markdown
[![Codecov](https://codecov.io/gh/neez777/sapFluxR/branch/main/graph/badge.svg)](https://codecov.io/gh/neez777/sapFluxR)
```

## Recommended Badge Progression

### Phase 1: Initial Upload (Use Now)
```markdown
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
```

### Phase 2: After GitHub Upload
Add:
```markdown
[![GitHub issues](https://img.shields.io/github/issues/neez777/sapFluxR)](https://github.com/neez777/sapFluxR/issues)
```

### Phase 3: After Setting Up CI/CD
Add:
```markdown
[![R-CMD-check](https://github.com/neez777/sapFluxR/workflows/R-CMD-check/badge.svg)](https://github.com/neez777/sapFluxR/actions)
```

### Phase 4: After Setting Up Testing
Add:
```markdown
[![Codecov](https://codecov.io/gh/neez777/sapFluxR/branch/main/graph/badge.svg)](https://codecov.io/gh/neez777/sapFluxR)
```

### Phase 5: After CRAN Submission (Future)
Add:
```markdown
[![CRAN status](https://www.r-pkg.org/badges/version/sapFluxR)](https://CRAN.R-project.org/package=sapFluxR)
[![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/sapFluxR)](https://CRAN.R-project.org/package=sapFluxR)
```

## Badge Customization

You can create custom badges at [shields.io](https://shields.io):

### Examples

**DOI Badge** (after getting DOI from Zenodo):
```markdown
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXX)
```

**Custom Status Badge**:
```markdown
[![Status](https://img.shields.io/badge/status-in%20development-yellow)](https://github.com/neez777/sapFluxR)
```

**Platform Badge**:
```markdown
[![Platform](https://img.shields.io/badge/platform-windows%20%7C%20macos%20%7C%20linux-lightgrey)](https://github.com/neez777/sapFluxR)
```

## Updating Lifecycle Badge

As your package matures, update the lifecycle badge:

**Experimental** (current):
```markdown
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
```

**Maturing** (once stable but still evolving):
```markdown
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
```

**Stable** (once API is locked):
```markdown
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
```

## Complete Badge Section Example

Here's a full example for later when you have everything set up:

```markdown
<!-- badges: start -->
[![Lifecycle: maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html#maturing)
[![R-CMD-check](https://github.com/neez777/sapFluxR/workflows/R-CMD-check/badge.svg)](https://github.com/neez777/sapFluxR/actions)
[![Codecov](https://codecov.io/gh/neez777/sapFluxR/branch/main/graph/badge.svg)](https://codecov.io/gh/neez777/sapFluxR)
[![CRAN status](https://www.r-pkg.org/badges/version/sapFluxR)](https://CRAN.R-project.org/package=sapFluxR)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.XXXXXX.svg)](https://doi.org/10.5281/zenodo.XXXXXX)
[![GitHub issues](https://img.shields.io/github/issues/neez777/sapFluxR)](https://github.com/neez777/sapFluxR/issues)
[![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/sapFluxR)](https://CRAN.R-project.org/package=sapFluxR)
<!-- badges: end -->
```

## Quick Reference

| Badge Type | When to Add | Setup Difficulty |
|------------|-------------|------------------|
| Lifecycle | ✅ Now | Easy - just add |
| License | ✅ Now | Easy - just add |
| Project Status | ✅ Now | Easy - just add |
| Version | ✅ Now | Easy - manual update |
| GitHub Issues | After upload | Easy - just add |
| R-CMD-check | After upload | Medium - needs GitHub Actions |
| Codecov | After tests | Medium - needs account |
| CRAN | After submission | Automatic once on CRAN |
| DOI | After Zenodo | Easy - just add |

## Your Current Badges Are Fine!

The badges now in your README are perfect for initial release:
- They work immediately
- They're informative
- They're professional
- They don't show errors

You can add more advanced badges later as you set up those services.

## Resources

- [shields.io](https://shields.io) - Create custom badges
- [R package badges](https://github.com/GuangchuangYu/badger) - R package for badges
- [usethis badges](https://usethis.r-lib.org/reference/badges.html) - Built-in badge functions
- [Lifecycle badges](https://lifecycle.r-lib.org/articles/stages.html) - Package lifecycle stages

---

**Summary:** Your README now has working badges that look professional. You can add more advanced badges (R-CMD-check, Codecov) later once you set up those services.
