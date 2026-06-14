---
title: twbparser Workbook Inspector
emoji: 📊
colorFrom: teal
colorTo: blue
sdk: docker
app_port: 7860
pinned: false
license: mit
---

# twbparser Workbook Inspector

Interactive inspector for Tableau `.twb` / `.twbx` workbooks, built on the
[twbparser](https://github.com/PrigasG/twbparser) R package.

Upload a workbook (or load the bundled demo) to explore its datasources,
fields, filters, calculated fields, SQL, dashboards, and packaged assets, and
download a **replication brief** for porting the workbook to another tool.

## Usage

1. Click **Choose file** and upload a `.twb` or `.twbx` (up to 100 MB by
   default — see `TWBPARSER_MAX_UPLOAD_MB`), or click **Load demo workbook**.
2. Browse the tabs: Overview, Replication Brief, Pages, Filters, Shelves,
   Fields, Datasources, Calculations, SQL, TWBX Assets, Validation.
3. Use the **Export** panel to download the replication brief (`.md`) or any
   table as CSV.

## Configuration

| Variable | Default | Purpose |
|---|---|---|
| `TWBPARSER_MAX_UPLOAD_MB` | `100` | Maximum upload size in megabytes. |

To pin a specific package version, set the `TWBPARSER_REF` build arg in the
`Dockerfile` (e.g. `v0.4.0`).

## ⚠️ Security note (public deployments)

This Space parses **user-uploaded** XML and ZIP (`.twbx`) content. On a public
deployment, treat uploads as untrusted: parsing arbitrary XML can be subject to
entity-expansion attacks, and `.twbx` archives are ZIP files that could contain
unexpected or oversized entries. The app caps upload size and cleans up
temporary files per session, but does not otherwise sandbox parsing. For
sensitive or high-traffic use, run behind authentication and/or in an isolated
environment.
