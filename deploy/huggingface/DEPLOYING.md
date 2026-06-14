# Deploying the twbparser app to a Hugging Face Docker Space

These files (`Dockerfile` + `README.md`) are everything a Hugging Face **Docker**
Space needs. The Space installs `twbparser` from GitHub and serves the bundled
Shiny app, so the Space repo stays tiny — it does **not** contain the package
source.

## Steps

1. Push your latest package changes to GitHub (`PrigasG/twbparser`). The Space
   installs from GitHub, so the app it runs is whatever is on the ref you pin.
2. Create a new Space: https://huggingface.co/new-space
   - **SDK:** Docker → *Blank*
   - Name it (e.g. `twbparser-inspector`).
3. Add the two files from this folder to the **root** of the Space repo:
   - `Dockerfile`
   - `README.md`  (the YAML header is the Space card — keep it at the top)
4. Commit/push. The Space builds the image and starts the app on port `7860`.

```bash
git clone https://huggingface.co/spaces/<you>/twbparser-inspector
cd twbparser-inspector
cp /path/to/twbparser/deploy/huggingface/Dockerfile .
cp /path/to/twbparser/deploy/huggingface/README.md .
git add Dockerfile README.md
git commit -m "Add twbparser Shiny app"
git push
```

## Pinning a version (recommended)

By default the Dockerfile installs `HEAD` of the GitHub repo. For reproducible
builds, set the build arg to a tag or commit:

```dockerfile
ARG TWBPARSER_REF=v0.4.0
```

or in the Space settings add a build arg `TWBPARSER_REF=v0.4.0`.

## Notes

- First build is slow (installs R packages); subsequent builds are cached.
- Increase the upload cap via a Space **variable** `TWBPARSER_MAX_UPLOAD_MB`.
- The free CPU Space tier is sufficient; large `.twbx` files need more RAM.
