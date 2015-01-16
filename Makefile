
GHC       = ghc
GHC_FLAGS = -package std -package lang

.PHONY: depend clean
.SUFFIXES: .o .hs .hi

.o.hi:
	@:
	
.hs.o:
	$(GHC) -c $< $(GHC_FLAGS)

SRCS =  MA.hs            \
	VM.hs            \
	VMOps.hs         \
	VMErr.hs         \
	VMHeap.hs        \
	VMIntp.hs        \
	WideOps.hs       \
	VMStack.hs       \
	VMMonad.hs       \
	ArithOps.hs      \
	ArrayOps.hs      \
	BasicOps.hs      \
	BitUtils.hs      \
	OpenFile.hs      \
	ClassRep.hs      \
	ClassInit.hs     \
	RefSolver.hs     \
	BranchOps.hs     \
	ClassParser.hs   \
	ClassLoader.hs   \
	VMMain.hs

OBJS = $(SRCS:.hs=.o)

depend:
	ghc -M $(GHC_FLAGS) $(SRCS)

vmmain: $(OBJS)
	@rm -f $@
	$(GHC) -o $@ $(GHC_FLAGS) $(OBJS)
	@strip $@

clean:
	@rm -f *.o *.hi

# DO NOT DELETE: Beginning of Haskell dependencies
MA.o : MA.hs
MA.o : ClassRep.hi-boot
VM.o : VM.hs
VM.o : ./RefSolver.hi
VM.o : ./ClassLoader.hi
VM.o : ./ClassInit.hi
VM.o : ./ClassRep.hi
VM.o : ./VMMonad.hi
VM.o : ./VMStack.hi
VM.o : ./VMIntp.hi
VM.o : ./VMHeap.hi
VM.o : ./VMErr.hi
VM.o : ./MA.hi
VMOps.o : VMOps.hs
VMOps.o : VM.hi-boot
VMOps.o : ./ClassLoader.hi
VMOps.o : ./RefSolver.hi
VMOps.o : ./ClassInit.hi
VMOps.o : ./ClassRep.hi
VMOps.o : ./BasicOps.hi
VMOps.o : ./BitUtils.hi
VMOps.o : ./VMMonad.hi
VMOps.o : ./VMStack.hi
VMOps.o : ./VMHeap.hi
VMOps.o : ./VMErr.hi
VMOps.o : ./MA.hi
VMErr.o : VMErr.hs
VMErr.o : ./ClassRep.hi
VMHeap.o : VMHeap.hs
VMHeap.o : ClassRep.hi-boot
VMHeap.o : ./MA.hi
VMIntp.o : VMIntp.hs
VMIntp.o : VM.hi-boot
VMIntp.o : ./BranchOps.hi
VMIntp.o : ./BasicOps.hi
VMIntp.o : ./ArrayOps.hi
VMIntp.o : ./ArithOps.hi
VMIntp.o : ./WideOps.hi
VMIntp.o : ./VMOps.hi
VMIntp.o : ./VMMonad.hi
WideOps.o : WideOps.hs
WideOps.o : ./BasicOps.hi
WideOps.o : ./VMOps.hi
WideOps.o : ./BitUtils.hi
WideOps.o : ./VMStack.hi
WideOps.o : ./VMHeap.hi
VMStack.o : VMStack.hs
VMStack.o : ./ClassRep.hi
VMStack.o : ./VMHeap.hi
VMStack.o : ./MA.hi
VMMonad.o : VMMonad.hs
VMMonad.o : ./VMErr.hi
ArithOps.o : ArithOps.hs
ArithOps.o : VM.hi-boot
ArithOps.o : ./BasicOps.hi
ArithOps.o : ./BitUtils.hi
ArithOps.o : ./VMMonad.hi
ArithOps.o : ./VMStack.hi
ArithOps.o : ./VMHeap.hi
ArithOps.o : ./VMErr.hi
ArrayOps.o : ArrayOps.hs
ArrayOps.o : VM.hi-boot
ArrayOps.o : ./BasicOps.hi
ArrayOps.o : ./VMOps.hi
ArrayOps.o : ./ClassRep.hi
ArrayOps.o : ./BitUtils.hi
ArrayOps.o : ./VMMonad.hi
ArrayOps.o : ./VMStack.hi
ArrayOps.o : ./VMHeap.hi
ArrayOps.o : ./VMErr.hi
BasicOps.o : BasicOps.hs
BasicOps.o : VM.hi-boot
BasicOps.o : ./BitUtils.hi
BasicOps.o : ./VMMonad.hi
BasicOps.o : ./VMStack.hi
BasicOps.o : ./VMHeap.hi
BitUtils.o : BitUtils.hs
OpenFile.o : OpenFile.hs
OpenFile.o : ./VMMonad.hi
OpenFile.o : ./VMErr.hi
ClassRep.o : ClassRep.hs
ClassRep.o : ./VMHeap.hi
ClassRep.o : ./MA.hi
ClassInit.o : ClassInit.hs
ClassInit.o : VM.hi-boot
ClassInit.o : ./ClassRep.hi
ClassInit.o : ./VMMonad.hi
ClassInit.o : ./VMStack.hi
ClassInit.o : ./MA.hi
RefSolver.o : RefSolver.hs
RefSolver.o : ./ClassLoader.hi
RefSolver.o : ./ClassRep.hi
RefSolver.o : ./VMMonad.hi
RefSolver.o : ./VMErr.hi
RefSolver.o : ./MA.hi
BranchOps.o : BranchOps.hs
BranchOps.o : VM.hi-boot
BranchOps.o : ./BasicOps.hi
BranchOps.o : ./BitUtils.hi
BranchOps.o : ./VMStack.hi
BranchOps.o : ./VMHeap.hi
ClassParser.o : ClassParser.hs
ClassParser.o : ./ClassRep.hi
ClassParser.o : ./BitUtils.hi
ClassParser.o : ./VMErr.hi
ClassLoader.o : ClassLoader.hs
ClassLoader.o : ./ClassParser.hi
ClassLoader.o : ./ClassRep.hi
ClassLoader.o : ./OpenFile.hi
ClassLoader.o : ./VMMonad.hi
ClassLoader.o : ./VMErr.hi
ClassLoader.o : ./MA.hi
VMMain.o : VMMain.hs
VMMain.o : ./VMMonad.hi
VMMain.o : ./VM.hi
# DO NOT DELETE: End of Haskell dependencies
