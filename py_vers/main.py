import asyncio

import discord
from discord.ext import commands

cogs = [
    "cogs.stuff",
    "cogs.ext"
]


class SelfBot(commands.Bot):

    def __init__(self, config, *args, **kwargs):
        super().__init__(*args, self_bot=True,
                         command_prefix="|>",
                         description="wew_lad")

        self.config = config

    async def log(self, *args, **kwargs):
        chn = self.get_channel(self.config["log_channel"])
        if chn is not None:
            await chn.send(*args, **kwargs)
        else:
            print(*args, kwargs)

    async def on_ready(self):
        succ, fail = await self.load_cogs()
        await self.log("Loaded cogs\n"
                       f"Succ: ```{succ}```\n"
                       f"Fail: ```{fail}```")

    async def load_cogs(self):
        for extension in self.extensions.copy():
            self.unload_extension(extension)

        imported_modules = []
        failed_modules = []

        for i in cogs:
            try:
                self.load_extension(i)
            except Exception:
                failed_modules.append([i, traceback.format_exc()])
                print(traceback.format_exc())
            else:
                imported_modules.append(i)

        return imported_modules, failed_modules
