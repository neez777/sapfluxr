# GitHub Upload Checklist for sapFluxR

This guide helps you decide what to upload to GitHub by removing items from the comprehensive `.gitignore` file.

## ✅ Essential Files to Upload (Remove from .gitignore)

### Core Package Structure
- [ ] `R/` - All R source code files (**ESSENTIAL**)
- [ ] `man/` - Function documentation (**ESSENTIAL**)
- [ ] `DESCRIPTION` - Package metadata (**ESSENTIAL**)
- [ ] `NAMESPACE` - Package namespace (**ESSENTIAL**)
- [ ] `LICENSE` or `LICENSE.md` - License file (**ESSENTIAL**)
- [ ] `README.md` - Package introduction (**ESSENTIAL**)
- [ ] `NEWS.md` - Version history (Recommended)

### Package Configuration
- [ ] `.Rbuildignore` - Controls what R CMD build ignores (Recommended)
- [ ] `.gitignore` - Git ignore rules (you're creating this now!)

### Additional Directories (if applicable)
- [ ] `tests/` - Test files (Recommended for quality)
- [ ] `vignettes/` - Long-form documentation (Recommended)
- [ ] `data/` - Package datasets (if you have public data)
- [ ] `inst/` - Installed files (configurations, examples, etc.)
  - [ ] `inst/configurations/` - YAML config files
  - [ ] `inst/methods/` - Method definition files
  - [ ] `inst/extdata/` - Example data files

## 🤔 Optional Documentation (Decide what to share)

### Project Documentation
- [ ] `CLAUDE.md` - Development guide for Claude Code (Useful for contributors)
- [ ] `FUNCTION_REFERENCE.md` - Complete function reference (Useful!)
- [ ] `FUNCTION_FLOW_GUIDE.md` - Guide to function flow diagram (Useful!)
- [ ] `user_inputs.md` - Parameter guide (Very useful!)
- [ ] `CONFIGURATION_GUIDE.md` - How to configure the package (Useful!)
- [ ] `README_YAML_METHODS.md` - YAML method development (Useful!)
- [ ] `MIGRATION_GUIDE.md` - Migration instructions (if relevant)
- [ ] `QUICK_REFERENCE_CARD.md` - Quick reference (Useful!)

### Diagrams
- [ ] `sapFluxR_workflow.mmd` - Workflow diagram (**Recommended**)
- [ ] `sapFluxR_function_flow.mmd` - Function flow diagram (**Recommended**)

### Development Documentation (Probably keep private initially)
- [ ] `PROBE_CONFIG_REFACTOR_PLAN.md` - Internal planning
- [ ] `PARAMETER_CATEGORIZATION.md` - Internal organization
- [ ] `IMPLEMENTATION_PLAN.md` - Internal planning
- [ ] `VARIABLE_REFERENCE.md` - Internal reference
- [ ] `REFACTOR_CHECKLIST.md` - Internal checklist
- [ ] `REFACTOR_SUMMARY.md` - Internal summary
- [ ] `pseudo.md` - Pseudocode (internal development)
- [ ] `.dev/` directory - Development files

## 🚫 Files to Keep Ignored (Leave in .gitignore)

### Always Keep Private
- [ ] `.Rhistory` - R command history
- [ ] `.RData` - R workspace data
- [ ] `.Rproj.user/` - RStudio user settings
- [ ] `*.Rproj` - RStudio project files (or upload if you want)
- [ ] `.Renviron` - Environment variables (may contain secrets!)
- [ ] `.secrets` - Any secrets or credentials
- [ ] API keys, tokens, credentials

### Build Artifacts
- [ ] `*.tar.gz` - Built package files
- [ ] `*.Rcheck/` - Check output directories
- [ ] `vignettes/*.html` - Built vignette HTML (auto-generated)
- [ ] `vignettes/*.pdf` - Built vignette PDFs (auto-generated)
- [ ] `man/*.Rd` files if using roxygen2 (auto-generated)

### Personal Files
- [ ] `NOTES.md` - Personal notes
- [ ] `TODO.md` - Personal to-do lists
- [ ] Operating system files (.DS_Store, Thumbs.db, etc.)

## 📋 Step-by-Step Process

### 1. Review Your .gitignore
Open `.gitignore` and review each section.

### 2. Remove Lines for Files to Upload

For a typical R package, remove these lines from `.gitignore`:

```gitignore
# Remove these lines (commented with #):
# R/
# inst/
# man/
# vignettes/
# tests/
# data/
# DESCRIPTION
# NAMESPACE
# LICENSE
# LICENSE.md
# README.md
# NEWS.md
# .Rbuildignore
```

### 3. Decide on Documentation

Consider your audience:
- **Public package users**: Upload README.md, user_inputs.md, vignettes
- **Contributors/developers**: Upload CLAUDE.md, FUNCTION_REFERENCE.md
- **Minimal release**: Just core package files + README.md

### 4. Decide on Configuration Files

For `inst/configurations/` and `inst/methods/`:
- If these are examples/templates for users → Upload them
- If they contain your specific research data → Keep private

### 5. Check for Sensitive Data

Before uploading, search for:
```bash
# File paths that might be specific to your computer
grep -r "C:/Users/neez" .
grep -r "C:\\\\Users\\\\neez" .

# Potential passwords or keys
grep -ri "password" .
grep -ri "api_key" .
grep -ri "token" .
```

### 6. Initialize Git (if not already done)

```bash
cd "C:/Users/neez/OneDrive/Uni/VLS550/R/sapFluxR"
git init
git add .
git status --ignored  # Review what will be committed
```

### 7. Review What Will Be Uploaded

```bash
git status  # See what will be committed
git diff --cached  # Review changes
```

### 8. Create Initial Commit

```bash
git commit -m "Initial commit of sapFluxR package"
```

### 9. Create GitHub Repository

1. Go to github.com and create a new repository
2. **DO NOT** initialize with README (you already have one)
3. Copy the repository URL

### 10. Push to GitHub

```bash
git remote add origin https://github.com/YOUR_USERNAME/sapFluxR.git
git branch -M main
git push -u origin main
```

## 🎯 Recommended Minimal Upload

For a first public release, remove these items from `.gitignore`:

**Essential:**
- `R/` - Source code
- `man/` - Documentation
- `DESCRIPTION`, `NAMESPACE`, `LICENSE`, `README.md`
- `.Rbuildignore`, `.gitignore`

**Highly Recommended:**
- `tests/` - Shows your code is tested
- `vignettes/` - Helps users learn
- `NEWS.md` - Version history
- `inst/configurations/` - Example configs
- `inst/methods/` - Method definitions
- `user_inputs.md` - Parameter guide
- `*.mmd` files - Diagrams

**Optional:**
- Other documentation files (based on your preference)
- `data/` if you have example datasets

## 🔍 Final Checks

Before pushing:

- [ ] Review README.md - does it make sense for external users?
- [ ] Check LICENSE - is it correct?
- [ ] Remove any OneDrive/local paths from code
- [ ] Remove any personal notes from comments
- [ ] Verify no credentials/keys in any files
- [ ] Test that package builds: `devtools::check()`
- [ ] Check all examples run: `devtools::run_examples()`

## 📚 Additional Resources

- [R Packages Book](https://r-pkgs.org/) - Comprehensive guide
- [GitHub for R Packages](https://happygitwithr.com/) - Git + R integration
- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html) - If planning to submit to CRAN

## 🆘 If You Accidentally Upload Something

If you accidentally commit sensitive data:

```bash
# Remove file from git but keep local copy
git rm --cached SENSITIVE_FILE

# Commit the removal
git commit -m "Remove sensitive file"

# Force push (only if needed and you understand the implications)
git push --force
```

**Note:** Files in Git history remain in history! For truly sensitive data, you may need to use tools like `git filter-branch` or contact GitHub support.

---

**Date Created**: 2025-10-12
**Purpose**: Guide for initial GitHub upload of sapFluxR package
