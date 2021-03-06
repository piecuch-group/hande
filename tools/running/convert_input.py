#!/usr/bin/env python

# WARNING: this is a hack.  Only common (simple) cases are considered.

import sys
import collections

def read_old(filename):
    '''Read the old input file in and convert it to a dictionary.'''

    inp = {}
    comments = []
    idim = 0
    ndim = 3
    with open(filename) as f:
        for line in f:
            words = line.lower().split()
            if not words:
                pass
            elif line.strip().startswith('('):
                comments.append(line.strip()[1:-1])
            elif words[0] in ('read', 'dipole_integrals'):
                # don't lower-case filenames!
                inp[words[0]] = line.split()[1:]
            elif words[0] == 'lattice':
                inp['lattice'] = []
                # read the next lines containing the lattice block
                words = next(f).split()
                inp['lattice'].append(words)
                ndim = len(words)
                for i in range(1, ndim):
                    words = next(f).split()
                    inp['lattice'].append(words)
            elif words[0] == 'fci_rdm':
                inp[words[0]] = next(f).split()
            elif words[0] == 'select_reference_det':
                if len(words) == 1:
                    inp[words[0]] = None
                elif len(words) == 2:
                    inp[words[0]] = '{ update_every = %s }' % (words[1])
                else:
                    inp[words[0]] = '{ update_every = %s, pop_factor = %s }' % (words[1], words[2])
            elif words[0] in ('walker_length', 'spawned_walker_length', 'spawned_rdm_length'):
                words[0] = words[0].replace('walker_length', 'state_size')
                if words[-1] == 'mb':
                    words[1] = '-%s' % (words[1])
                inp[words[0]] = [words[1]]
            elif words[0] == 'dmqmc_weighted_sampling':
                inp[words[0]] = next(f).split()
            elif words[0] == 'reduced_density_matrix':
                inp[words[0]] = []
                for i in range(int(words[1])):
                    inp[words[0]].append(next(f).split())
            elif words[0] == 'attempt_spawn_prob':
                inp['pattempt_single'] = words[1]
                inp['pattempt_double'] = words[2]
            elif words[0] == 'neel_singlet_guiding':
                inp['guiding_function'] = ['neel_singlet']
            elif words[0] == 'neel_singlet_estimator':
                inp['trial_function'] = ['neel_singlet']
            elif words[0] == 'dump_restart':
                if len(words) == 1:
                    inp[words[0]] = None
                elif words[1] == 'shift':
                    if len(words) == 2:
                        inp['write_shift'] = None
                    else:
                        inp['write_shift'] = words[2:]
                else:
                    inp[words[0]] = words[1:]
            elif words[0] == 'print_fci_wfn':
                if len(words) == 1:
                    inp['write_nwfns'] = [-1]
                else:
                    inp['write_nwfns'] = [words[1]]
                    if len(words) == 3:
                        inp['wfn_file'] = [words[2]]
            elif words[0] == 'analyse_fci_wfn' and len(words) == 1:
                inp[words[0]] = [-1]
            elif words[0] in ('hamil', 'hamiltonian'):
                inp['write_hamiltonian'] = None
                if len(words) == 2:
                    inp['hamiltonian_file'] = [words[1]]
            elif words[0] in ('dets', 'determinants'):
                inp['write_determinants'] = None
                if len(words) == 2:
                    inp['determinant_file'] = [words[1]]
            elif len(words) == 1:
                if words[0] in ('iccmc', 'ifciqmc'):
                    words[0] = words[0][1:]
                    inp['initiator'] = None
                inp[words[0]] = None
            else:
                inp[words[0]] = words[1:]
    return (comments, inp)

def read_to_dict(inp, keys, remap, store):
    '''Select keys (and possibly remap them) from the input dictionary and copy them to the store.'''

    for key_orig in keys:
        # rename keys
        if key_orig in remap.keys():
            key = remap[key_orig]
        else:
            key = key_orig
        if key_orig in inp.keys():
            # hard-coded special cases
            if key_orig == '2d':
                store['dim'] = [2]
            elif key_orig == '3d':
                store['dim'] = [3]
            # general
            elif inp[key_orig]:
                store[key] = inp[key_orig]
            else:
                store[key] = ['true']
    return store

def get_sys(inp):
    '''Get the system settings from the input dictionary.'''

    system = collections.OrderedDict()

    for sys in 'hubbard_real hubbard_k heisenberg read ueg chung-landau'.split():
        if sys in inp.keys():
            system['type'] = sys
            break
    if system['type'] == 'chung-landau':
        system['type'] = 'chung_landau'
    if system['type'] == 'read':
        system['type'] = 'read_in'
        if inp['read']:
            system['int_file'] = inp['read']

    system_keys = ('electrons nel lattice ms sym u t j twist finite_cluster '
                   'triangular_lattice lz cas 2d 3d ecutoff rs chem_pot '
                   'dipole_integrals magnetic_field staggered_magnetic_field'.split())

    remap = {
                'finite_cluster':'finite',
                'ecutoff':'cutoff',
                'dipole_integrals':'dipole_int_file',
                'u': 'U',
                'j': 'J',
                'lz': 'Lz',
                'cas': 'CAS',
                'triangular_lattice': 'triangular',
            }

    read_to_dict(inp, system_keys, remap, system)

    return system

def get_calc(inp):
    '''Get the calculation settings from the input dictionary.'''

    remap = {
                'estimate_canonical_kinetic_energy':'kinetic_energy',
                'estimate_hilbert_space':'hilbert_space',
                'lanczos': 'fci',
                'lanczos_direct': 'fci',
            }

    calc_types = 'fci lanczos lanczos_direct estimate_hilbert_space estimate_canonical_kinetic_energy fciqmc ccmc simple_fciqmc dmqmc'.split()

    calcs = []
    for calc_type in calc_types:
        if calc_type in inp.keys():
            if calc_type in  remap.keys():
                calcs.append(remap[calc_type])
            else:
                calcs.append(calc_type)

    opts = {}

    opts['fci'] = collections.OrderedDict()
    keys = 'write_hamiltonian hamiltonian_file write_determinants determinant_file write_nwfns wfn_file analyse_fci_wfn block_size fci_rdm'.split()
    remap = {
                'fci_rdm': 'rdm',
                'block_size': 'blacs_block_size',
                'analyse_fci_wfn': 'nanalyse',
            }
    read_to_dict(inp, keys, remap, opts['fci'])

    opts['lanczos'] = collections.OrderedDict()
    keys = 'lanczos_basis lanczos_solutions lanczos_solns direct_lanczos sparse_hamiltonian'.split()
    remap = {
                'lanczos_basis': 'nbasis',
                'lanczos_solns': 'neigv',
                'lanczos_solutions': 'neigv',
                'direct_lanczos': 'direct',
                'sparse_hamiltonian': 'sparse',
            }
    read_to_dict(inp, keys, remap, opts['lanczos'])
    lanczos = 'lanczos' in inp.keys()
    # Toggled the default for sparse Hamiltonians...
    if lanczos and 'sparse' not in opts['lanczos'].keys():
        opts['lanczos']['sparse'] = ['false']

    opts['hilbert'] = collections.OrderedDict()
    keys = 'estimate_hilbert_space truncation_level seed reference'.split()
    remap = {
                'estimate_hilbert_space': 'ncycles',
                'truncation_level':'ex_level',
                'seed':'rng_seed',
            }
    read_to_dict(inp, keys, remap, opts['hilbert'])
    # legacy -- forgot to set ncycles for cases which don't go via Monte Carlo in a couple of the tests.
    if 'ncycles' in opts['hilbert']:
        if opts['hilbert']['ncycles'][0] == 'true':
            opts['hilbert']['ncycles'] = [0]

    opts['kinetic'] = collections.OrderedDict()
    keys = 'init_beta init_pop num_kinetic_cycles seed fermi_temperature'.split()
    remap = {
                'truncation_level':'ex_level',
                'seed':'rng_seed',
                'num_kinetic_cycles':'ncycles',
                'init_beta':'beta',
                'init_pop':'nattempts',
            }
    read_to_dict(inp, keys, remap, opts['kinetic'])

    opts['qmc'] = collections.OrderedDict()
    keys = 'tau initiator tau_search initial_shift seed init_pop mc_cycles nreports varyshift_target vary_shift_from real_amplitudes spawn_cutoff no_renorm shift_damping initiator_population pattempt_single pattempt_double state_size spawned_state_size use_mpi_barriers'.split()
    remap = {
                'varyshift_target': 'target_population',
                'initiator_population': 'initiator_threshold',
                'seed':'rng_seed',
            }
    read_to_dict(inp, keys, remap, opts['qmc'])

    opts['fciqmc'] = collections.OrderedDict()
    keys = 'non_blocking_comm load_balancing init_spin_inverse_reference_det guiding_function trial_function select_reference_det'.split()
    remap = {}
    read_to_dict(inp, keys, remap, opts['fciqmc'])

    opts['ccmc'] = collections.OrderedDict()
    keys = 'move_freq cluster_multispawn_threshold ccmc_linked ccmc_full_nc'.split()
    remap = {
                'ccmc_linked': 'linked',
                'ccmc_full_nc': 'full_non_composite',
            }
    read_to_dict(inp, keys, remap, opts['ccmc'])

    opts['dmqmc'] = collections.OrderedDict()
    keys = 'replica_tricks fermi_temperature use_all_sym_sectors use_all_spin_sectors beta_loops dmqmc_weighted_sampling find_weights half_density_matrix dmqmc_vary_weights'.split()
    remap = {
                'use_all_spin_sectors':'all_spin_sectors',
                'use_all_sym_sectors': 'all_sym_sectors',
                'dmqmc_weighted_sampling':'sampling_weights',
                'dmqmc_vary_weights':'vary_weights',
                'half_density_matrix':'symmetrize',
            }
    read_to_dict(inp, keys, remap, opts['dmqmc'])

    opts['ipdmqmc'] = collections.OrderedDict()
    keys = 'init_beta free_electron_trial grand_canonical_initialisation metropolis_attempts max_metropolis_move'.split()
    remap = {
                'init_beta': 'initial_beta',
                'max_metropolis_move':'max_metropolis_moves',
                'free_electron_trial':'free_electron_partition',
            }
    read_to_dict(inp, keys, remap, opts['ipdmqmc'])
    ipdmqmc = 'propagate_to_beta' in inp.keys()

    opts['operators'] = collections.OrderedDict()
    keys = 'dmqmc_full_renyi_2 dmqmc_energy dmqmc_energy_squared dmqmc_correlation_function dmqmc_staggered_magnetisation dmqmc_correlation_function output_excitation_distribution start_averaging_excitation_dist'.split()
    remap = {
                'dmqmc_full_renyi_2':'renyi2',
                'dmqmc_energy':'energy',
                'dmqmc_energy_squared':'energy2',
                'dmqmc_staggered_magnetisation':'staggered_magnetisation',
                'dmqmc_correlation_function':'correlation',
                'output_excitation_distribution':'excit_dist',
                'start_averaging_excitation_dist':'excit_dist_start',
            }
    read_to_dict(inp, keys, remap, opts['operators'])

    opts['rdm'] = collections.OrderedDict()
    keys = 'spawned_rdm_length reduced_density_matrix ground_start_rdm start_averaging_rdm instantaneous_rdm output_rdm concurrence von_neumann_entropy renyi_entropy_2'.split()
    remap = {
                'spawned_rdm_length':'spawned_state_size',
                'reduced_density_matrix':'rdms',
                'ground_rdm':'ground_state',
                'ground_start_rdm':'ground_start_start',
                'instantaneous_rdm':'instantaneous',
                'output_rdm':'write',
                'von_neumann_entropy':'von_neumann',
                'renyi_entropy_2':'renyi2',
            }
    read_to_dict(inp, keys, remap, opts['rdm'])

    opts['semi_stoch'] = collections.OrderedDict()
    keys = 'semi_stoch_high_pop semi_stoch_read write_determ_space semi_stoch_combine_annihil semi_stoch_shift_start semi_stoch_iteration'.split()
    remap = {
                'semi_stoch_high_pop': 'size',
                'semi_stoch_combine_annihil': 'separate_annihilation',
                'semi_stoch_iteration': 'start_iteration',
                'semi_stoch_shift_start': 'shift_start_iteration',
            }
    read_to_dict(inp, keys, remap, opts['semi_stoch'])
    # Need to do some juggling.
    # We've switched true and false on the semi_stoch_combine_annihil option.  Default is true.
    if 'separate_annihilation' in opts['semi_stoch']:
            opts['semi_stoch']['separate_annihilation'] = ['false']
    # And have a new option for specifying the space.
    if 'size' in opts['semi_stoch']:
        opts['semi_stoch']['space'] = ['high']
    elif 'semi_stoch_read' in opts['semi_stoch']:
        opts['semi_stoch']['space'] = ['read']
        opts['semi_stoch'].pop('semi_stoch_read')

    opts['load_bal'] = collections.OrderedDict()
    keys = 'load_balancing_slots load_balancing_pop percent_imbal max_load_attempts write_load_info'.split()
    remap = {
                'load_balancing_slots':'nslots',
                'load_balancing_pop':'min_pop',
                'percent_imbal':'target',
                'max_load_attempts': 'max_attempts',
                'write_load_info': 'write',
            }
    read_to_dict(inp, keys, remap, opts['load_bal'])

    opts['restart'] = collections.OrderedDict()
    keys = 'restart dump_restart write_shift dump_restart_frequency'.split()
    remap = {
                'dump_restart_frequency':'write_frequency',
                'dump_restart':'write',
                'restart':'read',
            }
    read_to_dict(inp, keys, remap, opts['restart'])

    opts['reference'] = collections.OrderedDict()
    keys = 'truncation_level reference_det hs_reference_det'.split()
    remap = {
                'truncation_level': 'ex_level',
                'reference_det': 'det',
                'hs_reference_det': 'hilbert_space_det',
            }
    read_to_dict(inp, keys, remap, opts['reference'])

    # options that go in the top-level table rather than in their own table.
    opts['top'] = collections.OrderedDict()
    keys = 'sparse_hamiltonian'.split()
    remap = { 'sparse_hamiltonian':'sparse' }
    read_to_dict(inp, keys, remap, opts['top'])
    # Toggled the default from old input...
    if 'sparse' not in opts['top'].keys():
        opts['top']['sparse'] = ['false']

    opts_slim = {}
    for (k, v) in opts.items():
        if v:
            opts_slim[k] = v
        elif (k == 'lanczos' and lanczos) or (k == 'ipdmqmc' and ipdmqmc):
            opts_slim[k] = v

    return (calcs, opts_slim)

def dict_to_table(d, indent=4, prefix=''):
    '''Convert a python dictionary to a lua table in string format, minus the {} delimiters.'''

    s = []
    if prefix:
        s.append(' '*(indent-4)+'%s {' % (prefix))
    space = ' '*indent
    for (key, val) in d.items():
        if key == 'lattice' or key == 'rdms':
            lattice = '{ {%s} }' % ('}, {'.join(', '.join(x) for x in val))
            s.append(space+'%s = %s,' % (key, lattice))
        elif len(val) == 1:
            try:
                x = float(val[0])
                s.append(space+'%s = %s,' % (key, val[0]))
            except ValueError:
                if val[0] in ['true', 'false']:
                    s.append(space+'%s = %s,' % (key, val[0]))
                else:
                    s.append(space+'%s = "%s",' % (key, val[0]))
        else:
            # have a (numerical) vector.
            s.append(space+'%s = {%s},' % (key, ', '.join(val)))
    if prefix:
        s.append(' '*(indent-4)+'},')
    return '\n'.join(s)

def print_new(comments, sys, calcs, opts):
    '''Print out the input file in lua format.'''

    print('sys = %s {' % sys['type'])
    sys.pop('type')
    print(dict_to_table(sys))
    print('}\n')

    for calc in calcs:
        print('%s {' % calc)
        print('    sys = sys,')
        if calc == 'fci':
            for table in ('fci', 'lanczos', 'reference'):
                if table in opts:
                    print(dict_to_table(opts[table], indent=8, prefix='%s =' % table))
        elif calc == 'hilbert_space':
            print(dict_to_table(opts['hilbert'], indent=8, prefix='hilbert ='))
        elif calc == 'kinetic_energy':
            print(dict_to_table(opts['kinetic'], indent=8, prefix='kinetic ='))
        else:
            print(dict_to_table(opts['qmc'], indent=8, prefix='qmc ='))
            if calc == 'fciqmc':
                for table in ('fciqmc', 'semi_stoch', 'load_bal'):
                    if table in opts:
                        print(dict_to_table(opts[table], indent=8, prefix='%s =' % table))
            elif calc == 'ccmc':
                if 'ccmc' in opts:
                    print(dict_to_table(opts['ccmc'], indent=8, prefix='ccmc ='))
            elif calc == 'simple_fciqmc':
                print(dict_to_table(opts['top']))
            elif calc == 'dmqmc':
                for table in ('dmqmc', 'ipdmqmc', 'operators', 'rdm'):
                    if table in opts:
                        print(dict_to_table(opts[table], indent=8, prefix='%s =' % table))
            for table in ('restart', 'reference'):
                if table in opts:
                    print(dict_to_table(opts[table], indent=8, prefix='%s =' % table))
        print('}')

    for comment in comments:
        print('--%s' % comment)

if __name__ == '__main__':

    (comments, inp) = read_old(sys.argv[1])
    sys = get_sys(inp)
    (calcs, opts) = get_calc(inp)
    print_new(comments, sys, calcs, opts)
