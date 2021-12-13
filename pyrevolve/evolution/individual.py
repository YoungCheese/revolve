  # (G,P)
import pickle

class Individual:
    def __init__(self, genotype, phenotype=None):
        """
        Creates an Individual object with the given genotype and optionally the phenotype.

        :param genotype: genotype of the individual
        :param phenotype (optional): phenotype of the individual
        """
        self.genotype = genotype
        self.phenotype = phenotype
        self.fitness = None
        # novelty is a measure of diversity
        self.novelty = None
        self.novelty_pop = None
        self.consolidated_fitness = None
        self.evaluated = False
        self.parents = None
        self.failed_eval_attempt_count = 0
        self.early_survival_probability = 0

    def develop(self, environment):
        """
        Develops genotype into a intermediate phenotype

        """
        if self.phenotype is None:
            self.phenotype = self.genotype.develop(environment)

    @property
    def id(self):
        _id = None
        if self.phenotype is not None:
            _id = self.phenotype.id
        elif self.genotype.id is not None:
            _id = self.genotype.id
        return _id

    def export_genotype(self, folder):
        self.genotype.export_genotype(f'{folder}/genotypes')

    def export_parents(self, folder):
        self.genotype.export_parents(f'{folder}')

    def export_phenotype(self, folder):
        if self.phenotype is not None:
            self.phenotype.save_file(f'{folder}/phenotypes/{self.phenotype.id}.yaml', conf_type='yaml')

    def export_fitness(self, folder, gen_num):
        """
        It's saving the fitness into a file. The fitness can be a floating point number or None
        :param folder: folder where to save the fitness
        """
        with open(f'{folder}/fitness_{gen_num}_{self.id}.txt', 'w') as f:
            f.write(str(self.fitness))

    def export_consolidated_fitness(self, folder, gen_num):
        with open(f'{folder}/consolidated_fitness_{gen_num}_{self.id}.txt', 'w') as f:
            f.write(str(self.consolidated_fitness))

    def export_novelty(self, folder, gen_num):
        with open(f'{folder}/novelty_{gen_num}_{self.id}.txt', 'w') as f:
            f.write(str(self.novelty))

    def export_novelty_pop(self, folder, gen_num):
        with open(f'{folder}/novelty_pop_{gen_num}_{self.id}.txt', 'w') as f:
            f.write(str(self.novelty_pop))

    def export_individual(self, folder):
        f = open(f'{folder}/individuals/individual_{self.id}.pkl', "wb")
        pickle.dump(self, f)
        f.close()

    def export(self, folder):
        self.export_genotype(folder)
        self.export_phenotype(folder)
        self.export_fitness(folder)

    def __repr__(self):
        return f'Individual_{self.id}({self.fitness})'
