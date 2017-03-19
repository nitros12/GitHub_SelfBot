import inspect
import re

import discord
from discord.ext import commands


class Stuff:

    def __init__(self, bot):
        self.bot = bot

    @commands.command()
    async def repl(self, ctx, *, lines: str):

        output = []
        env = {"bot": self.bot,
               "ctx": ctx,
               "print": (lambda *args, **kwargs: output.append("".join(str(i) for i in args)))
               }

        lines = lines.strip().strip("` ")

        res = exec(lines, globals(), env)
        if inspect.isawaitable(res):
            res = await res

        embed = discord.Embed(title="Repl command", colour=0xFFAAFF)
        embed.set_author(
            name=ctx.message.author, url="https://github.com/nitros12/SelfBot")

        embed.add_field(name="Input string", value=lines, inline=False)
        embed.add_field(name="Result", value="\n".join(output), inline=False)

        await ctx.message.edit(content=None, embed=embed)

    @commands.command()
    async def discon(self, ctx):
        await self.bot.close()


def setup(bot):
    bot.add_cog(Stuff(bot))
