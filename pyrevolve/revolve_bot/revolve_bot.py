"""
Revolve body generator based on RoboGen framework
"""
import yaml
import traceback
from collections import OrderedDict
from collections import deque

from pyrevolve import SDF
# from plasticoding import Alphabet

from .revolve_module import CoreModule, TouchSensorModule, Orientation
from .revolve_module import Orientation
from .brain import Brain, BrainNN

from .render.render import Render
from .render.brain_graph import BrainGraph
from .measure.measure_body import MeasureBody
from .measure.measure_brain import MeasureBrain

from ..custom_logging.logger import logger
import os


class RevolveBot:
    """
    Basic robot description class that contains robot's body and/or brain
    structures, ID and several other necessary parameters. Capable of reading
    a robot's sdf mode
    """

    def __init__(self, _id=None, self_collide=True):
        self._id = _id
        self._body = None
        self._brain = None
        self._morphological_measurements = None
        self._brain_measurements = None
        self._behavioural_measurements = None
        self.self_collide = self_collide
        self.battery_level = 0.0
        self.building_diff_unweighted = 0
        self.building_diff_weighted = 0
        self.substrate_coordinates_all = None
        self.substrate_coordinates_type = {}
        self.current_type = None
        self.cost_distinct_coord = 0.25
        self.cost_joint_to_joint = 0.01
        self.cost_joint_to_brick = 0.25
        # self.costdict = {'ST': 0, 'AJ1': 1, 'AJ2': 1.25, 'C': 0, 'B': 1.5}

    @property
    def id(self):
        return self._id

    @property
    def body(self):
        return self._body

    @property
    def brain(self):
        return self._brain

    def size(self):
        robot_size = 1 + self._recursive_size_measurement(self._body)
        return robot_size


    def _recursive_size_measurement(self, module):
        count = 0
        for _, child in module.iter_children():
            if child is not None:
                count += 1 + self._recursive_size_measurement(child)

        return count


    # recursively gets coordinates from all the non sensor modules and puts in dict with coordinates as key and module as value
    def _recursive_coord(self, module):
        if module.info['new_module_type']._value_ != 'ST':
            self.substrate_coordinates_type[(module.substrate_coordinates[0],module.substrate_coordinates[1] )] = module.info['new_module_type']._value_
        for _, child in module.iter_children():
            if child is not None:
                self._recursive_coord(child)

    # function that measures weighted and unweighted costs. adds cost to list, rotates one of the
    # dicts and measures again. adds lowest cost to attribute
    def measure_cost(self,planie):
        self._recursive_coord(self._body)
        planie._recursive_coord(planie._body)

        unweighted_costs = []
        weighted_costs = []
        unweighted_costs.append(self.symmetric_difference_unweighted(planie))
        weighted_costs.append(self.symmetric_difference_weighted(planie))
        for i in range(3):
            self.rotate()
            unweighted_costs.append(self.symmetric_difference_unweighted(planie))
            weighted_costs.append(self.symmetric_difference_weighted(planie))
        self.building_diff_weighted = min(weighted_costs)
        planie.building_diff_weighted = min(weighted_costs)
        self.building_diff_unweighted = min(unweighted_costs)
        # print(type(planie), 'planietype (RevolveBot')
        planie.building_diff_unweighted = min(unweighted_costs)
        return self.building_diff_unweighted, self.building_diff_weighted



    # rotates the grid of a robot. first entry becomes second entry of tuple, -1 * second becomes first.
    # flag to see if unweighted or weighted. this decides which dict to rotate
    def rotate(self):
        newdict = {}
        for key in self.substrate_coordinates_type:
            newkey = (-1 * key[1], key[0])
            newdict[newkey] = self.substrate_coordinates_type[key]
        # rotate the coordinates 90 degrees
        self.substrate_coordinates_type = newdict


    # loop over dict. if coordinates are the same, add difference of their cost (absolute value of subtracting them
    # from eachoter). else add cost for distinct coordinates. loop then over other dict (the plane robot)
    # most expensive is building a module from scratch, but also losing it completely
    # if there is a module but it changes, it is less expensive (half so expensive as spawning)
    ## todo take into account different modules between the two enviroments
    ## todo take into account which module is being spawned from thin air
    def symmetric_difference_weighted(self, planie):
        dif = 0
        list = []
        for key in self.substrate_coordinates_type:
            if key in planie.substrate_coordinates_type:
                list.append(key)
                self.current_type = self.substrate_coordinates_type[key]
                planie.current_type = planie.substrate_coordinates_type[key]
                if self.current_type != planie.current_type:
                    if (self.current_type == 'AJ1' and planie.current_type == 'AJ2') or (self.current_type == 'AJ2' and planie.current_type == 'AJ1'):
                        dif += self.cost_joint_to_joint
                    else:
                        dif += self.cost_joint_to_brick
            elif key in list:
                continue
            else:
                # or check which module is built
                dif += self.cost_distinct_coord

        for key in planie.substrate_coordinates_type:
            if key in self.substrate_coordinates_type and key not in list:
                self.current_type = self.substrate_coordinates_type[key]
                planie.current_type = planie.substrate_coordinates_type[key]
                if self.current_type != planie.current_type:
                    if (self.current_type == 'AJ1' and planie.current_type == 'AJ2') or (self.current_type == 'AJ2' and planie.current_type == 'AJ1'):
                        dif += self.cost_joint_to_joint
                    else:
                        dif += self.cost_joint_to_brick
            elif key in list:
                continue
            else:
                dif += self.cost_distinct_coord
        return dif


    # compares 2 grids by changing the grids to a set and check differences
    # via built in method
    def symmetric_difference_unweighted(self, planie):
        setstill = set(planie.substrate_coordinates_type)
        setrot = set(self.substrate_coordinates_type)
        dif = setstill ^ setrot
        dif = len(dif)
        dif = dif * self.cost_distinct_coord
        return dif


    def measure_behaviour(self):
        """

        :return:
        """
        pass

    def measure_phenotype(self, experiment_name):
        # make an attribute called cost measurements which depicts the difference of the 2 environemnts

        self._morphological_measurements = self.measure_body()
        self._brain_measurements = self.measure_brain()
        # measure the cost there
        # self._building_diff = self.measure_cost()

        # print('Robot ' + str(self.id) + ' was measured.')
        # logger.info('Robot ' + str(self.id) + ' was measured.')

    def measure_body(self):
        """
        :return: instance of MeasureBody after performing all measurements
        """
        if self._body is None:
            raise RuntimeError('Body not initialized')
        try:
            measure = MeasureBody(self._body)
            measure.measure_all()
            return measure
        except Exception as e:
            logger.exception('Failed measuring body')

    def export_phenotype_measurements(self, path, environment):
        # this one was from karine
        # file = open('experiments/' + path + '/data_fullevolution/' + environment + '/descriptors/'
        #           + 'phenotype_desc_' + str(self.id) + '.txt', 'a')
        file = open(path + '/data_fullevolution/' + environment + '/descriptors/'
                  + 'phenotype_desc_' + str(self.id) + '.txt', 'a')

        for key, value in self._morphological_measurements.measurements_to_dict().items():
            file.write('{} {}\n'.format(key, value))
        for key, value in self._brain_measurements.measurements_to_dict().items():
            file.write('{} {}\n'.format(key, value))
        file.write('{} {}\n'.format('unweighted_cost', self.building_diff_unweighted))
        file.write('{} {}\n'.format('weighted_cost', self.building_diff_weighted))

        file.close()



    # def export_cost(self, path, environment):
    #     with open('experiments/' + path + '/data_fullevolution/' + environment + '/descriptors/'
    #               + 'phenotype_desc_' + str(self.id) + '.txt', 'w+') as file:
    #
    #         file.write('{} {}\n'.format('unweighted cost', self.building_diff_unweighted))
    #         file.write('{} {}\n'.format('weighted cost', self.building_diff_weighted))
    #
    #     file.close()
    #         export here jasper

    def measure_brain(self):
        """
        :return: instance of MeasureBrain after performing all measurements
        """
        try:
            measure = MeasureBrain(self._brain, 10)
            measure = MeasureBrain(self._brain, 10)
            measure_b = MeasureBody(self._body)
            measure_b.count_active_hinges()
            if measure_b.active_hinges_count > 0:
                measure.measure_all()
            else:
                measure.set_all_zero()
            return measure
        except Exception as e:
            logger.exception('Failed measuring brain')

    def load(self, text, conf_type):
        """
        Load robot's description from a string and parse it to Python structure
        :param text: Robot's description string
        :param conf_type: Type of a robot's description format
        :return:
        """
        if 'yaml' == conf_type:
            self.load_yaml(text)
        elif 'sdf' == conf_type:
            raise NotImplementedError("Loading from SDF not yet implemented")

    def load_yaml(self, text):
        """
        Load robot's description from a yaml string
        :param text: Robot's yaml description
        """
        yaml_bot = yaml.safe_load(text)
        self._id = yaml_bot['id'] if 'id' in yaml_bot else None
        self._body = CoreModule.FromYaml(yaml_bot['body'])

        try:
            if 'brain' in yaml_bot:
                yaml_brain = yaml_bot['brain']
                if 'type' not in yaml_brain:
                    # raise IOError("brain type not defined, please fix it")
                    yaml_brain['type'] = 'neural-network'
                self._brain = Brain.from_yaml(yaml_brain)
            else:
                self._brain = Brain()
        except:
            self._brain = Brain()
            logger.exception('Failed to load brain, setting to None')

    def load_file(self, path, conf_type='yaml'):
        """
        Read robot's description from a file and parse it to Python structure
        :param path: Robot's description file path
        :param conf_type: Type of a robot's description format
        :return:
        """
        with open(path, 'r') as robot_file:
            robot = robot_file.read()

        self.load(robot, conf_type)

    def to_sdf(self, pose=SDF.math.Vector3(0, 0, 0.25), nice_format=None):
        if type(nice_format) is bool:
            nice_format = '\t' if nice_format else None
        return SDF.revolve_bot_to_sdf(self, pose, nice_format, self_collide=self.self_collide)

    def to_yaml(self):
        """
        Converts robot data structure to yaml

        :return:
        """
        yaml_dict = OrderedDict()
        yaml_dict['id'] = self._id
        yaml_dict['body'] = self._body.to_yaml()
        if self._brain is not None:
            yaml_dict['brain'] = self._brain.to_yaml()

        return yaml.dump(yaml_dict)

    def save_file(self, path, conf_type='yaml'):
        """
        Save robot's description on a given file path in a specified format
        :param path:
        :param conf_type:
        :return:
        """
        robot = ''
        if 'yaml' == conf_type:
            robot = self.to_yaml()
        elif 'sdf' == conf_type:
            robot = self.to_sdf(nice_format=True)

        with open(path, 'w') as robot_file:
            robot_file.write(robot)

    def update_substrate(self, raise_for_intersections=False):
        """
        Update all coordinates for body components

        :param raise_for_intersections: enable raising an exception if a collision of coordinates is detected
        :raises self.IntersectionCollisionException: If a collision of coordinates is detected (and check is enabled)
        """
        substrate_coordinates_map = {(0, 0): self._body.id}
        self._body.substrate_coordinates = (0, 0)
        self._update_substrate(raise_for_intersections, self._body, Orientation.NORTH, substrate_coordinates_map)

    class ItersectionCollisionException(Exception):
        """
        A collision has been detected when updating the robot coordinates.
        Check self.substrate_coordinates_map to know more.
        """

        def __init__(self, substrate_coordinates_map):
            super().__init__(self)
            self.substrate_coordinates_map = substrate_coordinates_map

    def _update_substrate(self,
                          raise_for_intersections,
                          parent,
                          parent_direction,
                          substrate_coordinates_map):
        """
        Internal recursive function for self.update_substrate()
        :param raise_for_intersections: same as in self.update_substrate
        :param parent: updates the children of this parent
        :param parent_direction: the "absolute" orientation of this parent
        :param substrate_coordinates_map: map for all already explored coordinates(useful for coordinates conflict checks)
        """
        dic = {Orientation.NORTH: 0,
               Orientation.WEST: 1,
               Orientation.SOUTH: 2,
               Orientation.EAST: 3}
        inverse_dic = {0: Orientation.NORTH,
                       1: Orientation.WEST,
                       2: Orientation.SOUTH,
                       3: Orientation.EAST}

        movement_table = {
            Orientation.NORTH: (1, 0),
            Orientation.WEST: (0, -1),
            Orientation.SOUTH: (-1, 0),
            Orientation.EAST: (0, 1),
        }

        for slot, module in parent.iter_children():
            if module is None:
                continue

            slot = Orientation(slot)

            # calculate new direction
            direction = dic[parent_direction] + dic[slot]
            if direction >= len(dic):
                direction = direction - len(dic)
            new_direction = Orientation(inverse_dic[direction])

            # calculate new coordinate
            movement = movement_table[new_direction]
            coordinates = (
                parent.substrate_coordinates[0] + movement[0],
                parent.substrate_coordinates[1] + movement[1],
            )
            module.substrate_coordinates = coordinates

            # For Karine: If you need to validate old robots, remember to add this condition to this if:
            # if raise_for_intersections and coordinates in substrate_coordinates_map and type(module) is not TouchSensorModule:
            if raise_for_intersections:
                if coordinates in substrate_coordinates_map:
                    raise self.ItersectionCollisionException(substrate_coordinates_map)
                substrate_coordinates_map[coordinates] = module.id


            self._update_substrate(raise_for_intersections,
                                   module,
                                   new_direction,
                                   substrate_coordinates_map)

    def _iter_all_elements(self):
        to_process = deque([self._body])
        while len(to_process) > 0:
            elem = to_process.popleft()
            for _i, child in elem.iter_children():
                if child is not None:
                    to_process.append(child)
            yield elem

    def render_brain(self, img_path):
        """
        Render image of brain
        @param img_path: path to where to store image
        """
        if self._brain is None:
            raise RuntimeError('Brain not initialized')
        elif isinstance(self._brain, BrainNN):
            try:
                brain_graph = BrainGraph(self._brain, img_path)
                brain_graph.brain_to_graph(True)
                brain_graph.save_graph()
            except Exception as e:
                logger.exception('Failed rendering brain. Exception:')
        else:
            raise RuntimeError('Brain {} image rendering not supported'.format(type(self._brain)))

    def render_body(self, img_path):
        """
        Render 2d representation of robot and store as png
        :param img_path: path of storing png file
        """
        if self._body is None:
            raise RuntimeError('Body not initialized')
        else:
            try:
                render = Render()
                render.render_robot(self._body, img_path)
            except Exception as e:
                logger.exception('Failed rendering 2d robot')

    def __repr__(self):
        return f'RevolveBot({self.id})'
