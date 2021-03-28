import sys
import fire
import os
from xml.dom.minidom import parse
import xmind




class XmindParser(object):
    def __init__(self, filename):
        self._filename = filename
        
        self._workbook = None
        self._data = None
        self._parse()
    
    def _parse(self):
        if not os.path.exists(self._filename):
            raise Exception("Can not find the file: %s" % self._filename)
        self._workbook = xmind.load(self._filename)
        self._data = self._workbook.getData()
        
    def _dump_topic(self, topic, indent='\t'):
        print('%s- %s' % (indent, topic.getTitle()))
        for topic in topic.getSubTopics() or []:
            self._dump_topic(topic, indent+'\t')

    def _dump_sheet(self, sheet):
        root_topic = sheet.getRootTopic()
        print("# %s" % root_topic.getTitle())
        for topic in root_topic.getSubTopics() or []:
            self._dump_topic(topic, '\t')

        for topic in root_topic.getSubTopics(xmind.core.const.TOPIC_DETACHED) or []:
            print("## %s" % topic.getTitle())
            for sub_topic in topic.getSubTopics() or []:
                self._dump_topic(sub_topic, '\t')

    def dump_markdown(self):
        for sheet in self._workbook.getSheets():
            self._dump_sheet(sheet)


def main():
    fire.Fire(XmindParser)

if __name__ == "__main__":
    main() 
