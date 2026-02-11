import glob
import re
import os

directory = 'flit/src'
files = glob.glob(os.path.join(directory, 'template_*.f90'))

# Pattern for the definition block
# Matches variants of spacing
pattern_def = r'#define\s+PASTE\(X\)\s+X\s*\n#define\s+PASTE2\(X\)\s+PASTE\(X\)_\s*\n#define\s+CONCATHELP\(X,\s*Y\)\s+PASTE2\(X\)Y\s*\n#define\s+CONCAT\(X,\s*Y\)\s+CONCATHELP\(X,\s*Y\)'

replacement_def = """#define CONCAT_HELPER(X, Y) X ## _ ## Y
#define CONCAT(X, Y)        CONCAT_HELPER(X, Y)"""

# Pattern for the undef block
pattern_undef = r'#undef\s+PASTE\s*\n#undef\s+PASTE2\s*\n#undef\s+CONCATHELP\s*\n#undef\s+CONCAT'
replacement_undef = """#undef CONCAT_HELPER
#undef CONCAT"""

count = 0
for filepath in files:
    with open(filepath, 'r') as f:
        content = f.read()
    
    new_content = re.sub(pattern_def, replacement_def, content)
    new_content = re.sub(pattern_undef, replacement_undef, new_content)
    
    if content != new_content:
        with open(filepath, 'w') as f:
            f.write(new_content)
        count += 1
        print(f"Fixed {filepath}")

print(f"Total files fixed: {count}")
