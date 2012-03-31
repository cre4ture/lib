
#include "Symbols.h"

Symbols* symbContext = new Symbols(NULL);

void beginNewSymbContext()
{
	Symbols *newc = new Symbols(symbContext);
	symbContext = newc;
}

void endSymbContext()
{
	Symbols *oldc = symbContext;
	symbContext = oldc->getParent();
	delete oldc;
}

