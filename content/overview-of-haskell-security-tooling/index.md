+++
title = "Overview of Haskell security tooling"
date = 2026-06-22
[taxonomies]
authors = ["Gautier DI FOLCO"]
categories = ["Security"]
tags = ["Tooling", "Infrastructure"]
+++

Here is an overview of the Haskell security infrastructure, detailing the security team, the advisory database, the libraries developed for security metadata, and how downstream tools leverage this data.

## The Haskell Security Response Team

The Haskell Security Response Team (SRT) manages the security reporting and advisory workflow for the Haskell ecosystem.

We formed the team to establish a formal process for handling vulnerabilities in Haskell packages, and we outlined our initial organization and objectives in the [Q2 2023 report](https://github.com/haskell/security-advisories/blob/main/reports/2023-07-10-ann-q2-report.md). Since then, we have been gradually moving the ecosystem from ad-hoc vulnerability handling toward a structured repository model.

## The Security Advisory Database

The central resource for security advisories is the [haskell/security-advisories](https://github.com/haskell/security-advisories/) repository. This database contains Markdown files documenting known vulnerabilities in packages hosted on Hackage. 

The repository produces several outputs for consumption by humans and machines:
1. **Human-readable portal**: The static website at [haskell.github.io/security-advisories/](https://haskell.github.io/security-advisories/) presents a searchable index of all advisories.
2. **OSV Export**: The `generated/osv-export` branch contains advisories formatted in the Open Source Vulnerabilities (OSV) schema. This allows integration into global vulnerability databases.
3. **Snapshot Export**: The `generated/snapshot-export` branch provides database snapshots designed for offline tools to synchronize.

## Security Data Libraries

Within the `code/` subdirectory of the advisory repository, we develop several libraries to handle security data programmatically:
* **`hsec-core`**: Defines the data structures for security advisories and handles their parsing and validation.
* **`hsec-tools`**: Provides utility executables for database maintainers, including querying a security advisories database.
* **`hsec-sync`**: A tool to synchronize the local advisory cache with the remote database snapshots.
* **`osv`**: Implements serialization and deserialization for the OSV format.
* **`cvss`**: Implements scoring and parsing for the Common Vulnerability Scoring System.
* **`purl`**: Handles Package URL parsing and generation, which uniquely identifies package names and versions.

## Ecosystem Integration

Several downstream tools utilize this security data to protect Haskell applications:
* **[cabal-audit](https://github.com/MangoIV/cabal-audit/)**: A command-line tool that parses Cabal project files or build plans and queries the database to report vulnerable dependencies in a project.
* **[cabal-plan-submit](https://github.com/dancewithheart/cabal-plan-submit)**: A tool that extracts the build plan and submits the dependency graph to GitHub's Dependency Submission API.
* **[haskell-security-action](https://github.com/blackheaven/haskell-security-action)**: A GitHub Action that runs `cabal-audit` during CI runs and formats the output into SARIF. This allows vulnerability warnings to be displayed directly in the GitHub Code Scanning interface.

## Tutorial: Synchronizing and Querying

The tools above are built on the same libraries you can use yourself. If you want to query the advisory database directly, without going through a wrapper, the `hsec-tools` and `hsec-sync` executables let you do so from the command line.

First, initialize/synchronize the local cache with the remote advisory database:

```bash
$ hsec-sync sync
```

Once the cache is synchronized, you can query whether a specific package is affected by known vulnerabilities. For example, to check the `aeson` package:

```bash
$ hsec-tools query is-affected aeson
Affected by:
* [HSEC-2023-0001] Hash flooding vulnerability in aeson
* [HSEC-2026-0007] Denial of Service and Memory Exhaustion in aeson and text-iso8601
```

Because these packages are published on Hackage, developers can import `hsec-core` and query the advisory database programmatically in Haskell code without relying on the CLI.

## Future Developments

Our current goal is to improve the developer experience by tightening integration with core tooling. This includes:
* **Cabal integration**: Providing native audit commands directly in `cabal` so that developers do not need external wrappers, similarly to `npm`/`cargo`.
* **Hackage server integration**: Introducing fetching and checking advisories in `hackage-server` to warn maintainers and users about vulnerabilities directly on the Hackage package pages.

To learn more about the Security Response Team, report a vulnerability, or get in touch, visit [haskell.org/security](https://haskell.org/security).
