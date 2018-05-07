VERSIONS:=APS0 APS1 APS2 APS3

all:
	@for v in $(VERSIONS); do $(MAKE) -C $$v; done

clean:
	@for v in $(VERSIONS); do $(MAKE) -C $$v clean; done

mrproper:
	@for v in $(VERSIONS); do $(MAKE) -C $$v mrproper; done

checktype: all
	./checktype0.sh
	./checktype1.sh
	./checktype2.sh
	./checktype3.sh

evaluate: all
	./evaluate0.sh
	./evaluate1.sh
	./evaluate2.sh
	./evaluate3.sh
