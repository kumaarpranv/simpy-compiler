import numbers
import enum


class EnumComp(enum.Enum):
    def __gt__(self, other):
        try:
            return self.value > other.value
        except:
            pass
        try:
            if isinstance(other, numbers.Real):
                return self.value > other
        except:
            pass
        return NotImplemented

    def __lt__(self, other):
        try:
            return self.value < other.value
        except:
            pass
        try:
            if isinstance(other, numbers.Real):
                return self.value < other
        except:
            pass
        return NotImplemented

    def __ge__(self, other):
        try:
            return self.value >= other.value
        except:
            pass
        try:
            if isinstance(other, numbers.Real):
                return self.value >= other
            if isinstance(other, str):
                return self.name == other
        except:
            pass
        return NotImplemented

    def __le__(self, other):
        try:
            return self.value <= other.value
        except:
            pass
        try:
            if isinstance(other, numbers.Real):
                return self.value <= other
            if isinstance(other, str):
                return self.name == other
        except:
            pass
        return NotImplemented

    def __eq__(self, other):
        if self.__class__ is other.__class__:
            return self == other
        try:
            return self.value == other.value
        except:
            pass
        try:
            if isinstance(other, numbers.Real):
                return self.value == other
            if isinstance(other, str):
                return self.name == other
        except:
            pass
        return NotImplemented
