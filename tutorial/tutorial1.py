#!/usr/bin/env python3
import asyncio
import os
from pyrevolve.custom_logging.logger import logger
from pyrevolve.gazebo.manage import WorldManager as World
from pyrevolve.util.supervisor.supervisor_multi import DynamicSimSupervisor


async def run():
    logger.info('Hello World!')

    # Start Simulator
    simulator_supervisor = DynamicSimSupervisor(
        world_file='worlds/plane.world',
        simulator_cmd='gazebo',
        simulator_args=["--verbose"],
        plugins_dir_path=os.path.join('.', 'build', 'lib'),
        models_dir_path=os.path.join('.', 'models'),
        simulator_name='gazebo'
    )
    await simulator_supervisor.launch_simulator()
    await asyncio.sleep(0.1)

    connection = await World.create()
    if connection:
        logger.info("Connected to the simulator world.")
        await asynctio.sleep(5)
