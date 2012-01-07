def symmetry_copies(cmdname, args):
    import SymmetryCopies
    SymmetryCopies.symmetry_copies_command(cmdname, args)

def undo_symmetry_copies(cmdname, args):
    import SymmetryCopies
    SymmetryCopies.undo_symmetry_copies_command(cmdname, args)
    
import Midas.midas_text
Midas.midas_text.addCommand('sym', symmetry_copies, undo_symmetry_copies)
