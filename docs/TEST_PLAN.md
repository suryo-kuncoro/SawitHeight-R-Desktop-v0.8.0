# Test Plan v0.8.0

## Static
- node --check main/preload/renderer
- project verifier
- packaged-resource verifier
- registration backend structural delimiter scan

## Registration functional test on Windows R
1. Create a small target LAS copy shifted by known XYZ offset.
2. Prepare >=4 matched controls from original/shifted coordinates.
3. Run `TRANSLATION_3D`.
4. Verify output RMSE approaches quantization/control precision and transform recovers known shift.
5. Repeat with a known rigid transform and `RIGID_3D`.
6. Confirm RMSE gate blocks output when set below achieved LOO RMSE.
7. Confirm registered LAZ can be opened by lidR and used by Process One Period.

## Workflow
- Good PPK period: skip registration → Process → Compare.
- Bad period: Registration → registered LAS → Process → Compare.
- Next period: process only new period, then compare previous/current package.

## Live log
- Confirm registration chunk messages appear.
- Confirm heartbeat continues during long LAS transform/write.
- Confirm cancel terminates child process tree.
